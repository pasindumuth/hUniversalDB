{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Control.Monad as Mo
import qualified Data.Default as D
import qualified Data.Map as Mp
import qualified Data.Set as St
import qualified Data.Functor.Const
import qualified Data.Sequence as Sq
import qualified System.Environment as E
import qualified System.Random as R
import Text.Pretty.Simple (pPrintNoColor)

import qualified Connections as CC
import qualified Logging as L
import qualified MultiPaxosInstance as MP
import qualified InputActionHandler as IAH
import qualified PaxosLog as PL
import qualified MessageHandler as MH
import qualified Records.Actions.Actions as A
import qualified Records.GlobalState as GS
import qualified Records.Env as EN
import qualified Records.Messages.ClientMessages as CM
import qualified Records.Messages.Messages as M
import qualified Records.Messages.PaxosMessages as PM
import qualified Utils as U
import Lens (makeLenses, (%~), (.~), (^.), (&), (?~), at, ix, lp2, lp3, _1, _2,)
import State

type Queues = Mp.Map CC.EndpointId (Mp.Map CC.EndpointId (Sq.Seq M.Message))
type NonemptyQueues = St.Set (CC.EndpointId, CC.EndpointId)

data GlobalState = GlobalState {
  _slaveEIds :: [CC.EndpointId], -- EndpointIds for all slaves in the system
  _clientEIds :: [CC.EndpointId], -- EndpointIds for all client's we use for testing
  -- `queues` contains 2 queues (in for each direction) for every pair of
  -- for both client EndpointIds and slave Endpoints.
  _queues :: Queues,
  -- We use pairs of endpoints as identifiers of a queue. `nonemptyQueues`
  -- contain all queue IDs where the queue is non-empty
  _nonemptyQueues :: NonemptyQueues,
  _slaveState :: Mp.Map CC.EndpointId GS.GlobalState,
  _rand :: R.StdGen, -- Random Number Generator for simulation

  -- The following are everything related to asynchronous computation
  -- done at a node.
  _asyncQueues :: Mp.Map CC.EndpointId (Sq.Seq (A.InputAction, Int)),
  _clocks :: Mp.Map CC.EndpointId Int
} deriving (Show)

makeLenses ''GlobalState

mkSlaveEId i = "s" ++ show i
mkClientEId i = "c" ++ show i

createGlobalState :: Int -> Int -> Int -> GlobalState
createGlobalState seed numSlaves numClients =
  let slaveEIds = U.for [0..(numSlaves - 1)] $ mkSlaveEId
      clientEIds = U.for [0..(numClients - 1)] $ mkClientEId
      eIds = slaveEIds ++ clientEIds
      queues = Mp.fromList $ U.for eIds $ \eid1 ->
        (eid1, Mp.fromList $ U.for eIds $ \eid2 ->
        (eid2, Sq.empty))
      nonemptyQueues = St.empty
      rand = R.mkStdGen seed
      (slaves, rand') = U.s31 foldl ([], rand) slaveEIds $
        \(slaves, rand) eId ->
          let (r, rand')  = R.random rand
              g = D.def & GS.env . EN.rand .~ (R.mkStdGen r) & GS.env . EN.slaveEIds .~ slaveEIds
          in ((eId, g):slaves, rand')
      slaveState = Mp.fromList slaves
      asyncQueues = Mp.fromList $ U.for slaveEIds $ \eId -> (eId, Sq.empty)
      clocks = Mp.fromList $ U.for slaveEIds $ \eId -> (eId, 0)
  in GlobalState {
      _slaveEIds = slaveEIds,
      _clientEIds = clientEIds,
      _queues = queues,
      _nonemptyQueues = nonemptyQueues,
      _slaveState = slaveState,
      _rand = rand',
      _asyncQueues = asyncQueues,
      _clocks = clocks
    }

addMsg :: M.Message -> (CC.EndpointId, CC.EndpointId) -> ST GlobalState ()
addMsg msg (fromEId, toEId) = do
  queue <- queues . ix fromEId . ix toEId .^^.* U.push msg
  if Sq.length queue == 1
    then nonemptyQueues .^^. St.insert (fromEId, toEId)
    else nonemptyQueues .^^. id
  return ()

pollMsg :: (CC.EndpointId, CC.EndpointId) -> ST GlobalState M.Message
pollMsg (fromEId, toEId) = do
  msg <- queues . ix fromEId . ix toEId .^^* U.poll
  queueLength <- queues . ix fromEId . ix toEId .^^^* Sq.length
  if queueLength == 0
    then nonemptyQueues .^^. St.delete (fromEId, toEId)
    else nonemptyQueues .^^. id
  return msg

runIAction :: CC.EndpointId -> A.InputAction -> ST GlobalState ()
runIAction eId iAction = do
  (_, msgsO) <- runT (slaveState . ix eId) (IAH.handleInputAction iAction)
  Mo.forM_ msgsO $ \msgO -> do
    case msgO of
      A.Send toEIds msg -> Mo.forM_ toEIds $ \toEId -> addMsg msg (eId, toEId)
      A.RetryOutput counterValue -> do
        currentTime <- getT $ clocks . ix eId
        asyncQueues . ix eId .^^.* U.push (A.RetryInput counterValue, currentTime + 100)
        return ()
      _ -> return ()

deliverMessage :: (CC.EndpointId, CC.EndpointId) -> ST GlobalState ()
deliverMessage (fromEId, toEId) = do
  msg <- pollMsg (fromEId, toEId)
  runIAction toEId $ A.Receive fromEId msg

-- Simulate one millisecond of execution. This involves incrementing the slave's
-- clocks, handling any async tasks whose time has come to execute, and exchanging
-- `numMessages` number of messages.
simulateOnce :: Int -> ST GlobalState ()
simulateOnce numMessages = do
  -- increment each slave's clocks and run async tasks that are now ready
  eIds <- getL slaveEIds
  Mo.forM_ eIds $ \eId -> do
    randPercent :: Int <- rand .^^ R.randomR (1, 100)
    if randPercent <= 95
      then do
        clockVal <- clocks . ix eId .^^.* (+1)
        asyncQueue <- getT $ asyncQueues . ix eId
        let readyActions = U.s12 Sq.takeWhileL asyncQueue $ \(iAction, time) -> time == clockVal
        Mo.forM_ readyActions $ \(iAction, _) -> runIAction eId iAction
      else return ()
  -- Deliver `numMessages` messages
  Mo.forM_ [1..numMessages] $ \_ -> do
    length <- nonemptyQueues .^^^ St.size
    if length == 0
      then return ()
      else do
        randQueueIndex <- rand .^^ R.randomR (0, length - 1)
        queueId <- nonemptyQueues .^^^  St.elemAt randQueueIndex
        deliverMessage queueId

-- Simulate `n` milliseconds of execution
simulateN :: Int -> ST GlobalState ()
simulateN n = do
  numChans <- lp2 (slaveEIds, clientEIds) .^^^ \(s, c) -> (length s) + (length c)
  Mo.forM_ [1..n] $ \_ -> simulateOnce (numChans * (numChans + 1) `div` 2)

-- Simulate execution until there are no more messages in any channel
-- or any asyncQueue.
simulateAll :: ST GlobalState ()
simulateAll = do
  numChans <- lp2 (slaveEIds, clientEIds) .^^^ \(s, c) -> (length s) + (length c)
  let simulate =
        do
          length <- nonemptyQueues .^^^ St.size
          anyTasks <- asyncQueues .^^^ any (\q -> Sq.length q > 0)
          if length > 0 || anyTasks
            then do
              simulateOnce (numChans * (numChans + 1) `div` 2)
              simulate
            else return ()
  simulate

addClientMsg :: Int -> Int -> ST GlobalState ()
addClientMsg slaveId cliengMsgId = do
  let (clientEId, slaveEId) = (mkClientEId 0, mkSlaveEId slaveId)
      msg = M.MultiPaxosMessage $ PM.Insert $ PM.Write ("key " ++ show cliengMsgId) ("value " ++ show cliengMsgId) 1
  addMsg msg (clientEId, slaveEId)
  return ()

test1 :: ST GlobalState ()
test1 = do
  addClientMsg 0 0; simulateAll

test2 :: ST GlobalState ()
test2 = do
  addClientMsg 0 0; simulateN 2
  addClientMsg 1 1; simulateN 2
  addClientMsg 2 2; simulateN 2
  addClientMsg 3 3; simulateN 2
  addClientMsg 4 4; simulateN 2

mergePaxosLog :: GlobalState -> Maybe (Mp.Map PM.IndexT PM.PaxosLogEntry)
mergePaxosLog g =
  let paxosLogs = g ^. slaveState & Mp.toList & map (^. _2 . GS.paxosLog)
  in U.s31 foldl (Just Mp.empty) paxosLogs $
       \mergedLogM paxosLog ->
         case mergedLogM of
           Nothing -> Nothing
           _ ->
             U.s31 Mp.foldlWithKey mergedLogM (paxosLog & PL.plog) $
               \mergedLogM index value ->
                 case mergedLogM of
                   Nothing -> Nothing
                   Just mergedLog ->
                     case mergedLog ^. at index of
                       Nothing -> Just $ mergedLog & at index ?~ value
                       Just curValue -> if value == curValue
                                          then mergedLogM
                                          else Nothing

driveTest :: Int -> ST GlobalState () -> IO ()
driveTest testNum test = do
  let g = createGlobalState 0 5 1
      (_, (oActions, g')) = runST test g
  Mo.forM_ (reverse oActions) $ \action ->
    case action of
      A.Print message -> do
        print message
      _ -> return ()
  case mergePaxosLog g' of
    Just mergedLog -> print $
      "Test " ++ (show testNum) ++ " Passed! " ++ (show $ Mp.size mergedLog) ++ " entries."
    Nothing -> print $
      "Test " ++ (show testNum) ++ " Failed!"
  return ()

testDriver :: IO ()
testDriver = do
  driveTest 1 test1
  driveTest 2 test2

main :: IO ()
main = testDriver
