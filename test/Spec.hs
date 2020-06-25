{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

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
import State (ST, runST, get, addA, getA, (.^), (.^^), (.^^^), (.^^.), (.^^*), (.^^^*), wrapMaybe)

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
  _rand :: R.StdGen -- Random Number Generator for simulation
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
      (slaves, rand') = U.s13 foldl ([], rand) slaveEIds $
        \(slaves, rand) eId ->
          let (r, rand')  = R.random rand
              g = D.def & GS.env . EN.rand .~ (R.mkStdGen r) & GS.env . EN.slaveEIds .~ slaveEIds
          in ((eId, g):slaves, rand')
      slaveState = Mp.fromList slaves
   in GlobalState slaveEIds clientEIds queues nonemptyQueues slaveState rand'

addMsg :: M.Message -> (CC.EndpointId, CC.EndpointId) -> (Queues, NonemptyQueues) -> (Queues, NonemptyQueues)
addMsg msg (fromEId, toEId) (queues, nonemptyQueues) =
  let queues' = queues & ix fromEId %~ ix toEId %~ (Sq.|> msg)
      nonemptyQueues' =
        if (Sq.length $ queues' ^. ix fromEId . ix toEId) == 1
          then nonemptyQueues & St.insert (fromEId, toEId)
          else nonemptyQueues
  in (queues', nonemptyQueues')

deliverMessage :: (CC.EndpointId, CC.EndpointId) -> ST GlobalState ()
deliverMessage (fromEId, toEId) = do
  msg <- queues . ix fromEId . ix toEId .^^* U.poll
  queueLength <- queues . ix fromEId . ix toEId .^^^* Sq.length
  if queueLength == 0
    then nonemptyQueues .^^. St.delete (fromEId, toEId)
    else nonemptyQueues .^^. id
  (_, msgsO) <- getA (slaveState . at toEId) (wrapMaybe $ IAH.handleInputAction $ A.Receive fromEId msg)
  (lp2 (queues, nonemptyQueues)) .^^. (U.s31 foldl (reverse msgsO) $
    \(queues', nonemptyQueues') msgO ->
      case msgO of
        A.Send eIds msg -> U.s13 foldl (queues', nonemptyQueues') eIds $
          \(queues', nonemptyQueues') eId ->
            addMsg msg (toEId, eId) (queues', nonemptyQueues')
        _ -> (queues', nonemptyQueues'))
  return ()

simulateOnce :: ST GlobalState ()
simulateOnce = do
  length <- nonemptyQueues .^^^ St.size
  if length == 0
    then return ()
    else do
      randQueueIndex <- rand .^^ R.randomR (0, length - 1)
      queueId <- nonemptyQueues .^^^  St.elemAt randQueueIndex
      deliverMessage queueId

simulateN :: Int -> ST GlobalState ()
simulateN n = do
  length <- nonemptyQueues .^^^ St.size
  if length == 0 || n == 0
    then return ()
    else do
      simulateOnce
      simulateN (n - 1)

simulateAll :: ST GlobalState ()
simulateAll = do
  length <- nonemptyQueues .^^^ St.size
  if length == 0
    then return ()
    else do
      simulateOnce
      simulateAll

addClientMsg :: Int -> Int -> ST GlobalState ()
addClientMsg slaveId cliengMsgId = do
  let (clientEId, slaveEId) = (mkClientEId 0, mkSlaveEId slaveId)
      msg = M.MultiPaxosMessage $ PM.Insert $ PM.Write ("key " ++ show cliengMsgId) ("value " ++ show cliengMsgId) 1
  (lp2 (queues, nonemptyQueues)) .^^. addMsg msg (clientEId, slaveEId)
  return ()

test1 :: ST GlobalState ()
test1 = do
  addClientMsg 0 0; simulateAll

test2 :: ST GlobalState ()
test2 = do
  addClientMsg 0 0; simulateN 25
  addClientMsg 1 1; simulateN 25
  addClientMsg 2 2; simulateN 25
  addClientMsg 3 3; simulateN 25
  addClientMsg 4 4; simulateN 25

mergePaxosLog :: GlobalState -> Maybe (Mp.Map PM.IndexT PM.PaxosLogEntry)
mergePaxosLog g =
  let paxosLogs = g ^. slaveState & Mp.toList & map (^. _2 . GS.paxosLog)
  in U.s13 foldl (Just Mp.empty) paxosLogs $
       \mergedLogM paxosLog ->
         case mergedLogM of
           Nothing -> Nothing
           _ ->
             U.s13 Mp.foldlWithKey mergedLogM (paxosLog & PL.plog) $
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
