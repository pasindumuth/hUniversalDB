{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Control.Monad as Mo
import qualified Data.Default as Df
import qualified Data.List as Li
import qualified Data.Map as Mp
import qualified Data.Set as St
import qualified Data.Sequence as Sq
import qualified System.Random as Rn
import Text.Pretty.Simple (pPrintNoColor)

import qualified Infra.Utils as U
import qualified Paxos.MultiPaxosInstance as MP
import qualified Paxos.PaxosLog as PL
import qualified Proto.Actions.Actions as Ac
import qualified Proto.Common as Co
import qualified Proto.Messages as Ms
import qualified Proto.Messages.ClientMessages as CM
import qualified Proto.Messages.PaxosMessages as PM
import qualified Proto.Messages.TraceMessages as TrM
import qualified Slave.Tablet.TabletInputHandler as TIH
import qualified Slave.Tablet.Internal_MultiVersionKVStore as IMS
import qualified Slave.Tablet.Env as En
import qualified Slave.Tablet.TabletState as TS
import Infra.Lens
import Infra.State

type Queues = Mp.Map Co.EndpointId (Mp.Map Co.EndpointId (Sq.Seq Ms.Message))
type NonemptyQueues = St.Set (Co.EndpointId, Co.EndpointId)

data GlobalState = GlobalState {
  _slaveEIds :: [Co.EndpointId], -- EndpointIds for all slaves in the system
  _clientEIds :: [Co.EndpointId], -- EndpointIds for all client's we use for testing
  -- `queues` contains 2 queues (in for each direction) for every pair of
  -- for both client EndpointIds and slave Endpoints.
  _queues :: Queues,
  -- We use pairs of endpoints as identifiers of a queue. `nonemptyQueues`
  -- contain all queue IDs where the queue is non-empty
  _nonemptyQueues :: NonemptyQueues,
  _slaveState :: Mp.Map Co.EndpointId TS.TabletState,
  _rand :: Rn.StdGen, -- Random Number Generator for simulation

  -- The following are everything related to asynchronous computation
  -- done at a node.
  _asyncQueues :: Mp.Map Co.EndpointId (Sq.Seq (Ac.InputAction, Int)),
  _clocks :: Mp.Map Co.EndpointId Int
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
      rand = Rn.mkStdGen seed
      (slaves, rand') = U.s31 foldl ([], rand) slaveEIds $
        \(slaves, rand) eId ->
          let (r, rand')  = Rn.random rand
              g = Df.def & TS.env . En.rand .~ (Rn.mkStdGen r) & TS.env . En.slaveEIds .~ slaveEIds
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

addMsg :: Ms.Message -> (Co.EndpointId, Co.EndpointId) -> ST GlobalState ()
addMsg msg (fromEId, toEId) = do
  queue <- queues . ix fromEId . ix toEId .^^.* U.push msg
  if Sq.length queue == 1
    then nonemptyQueues .^^. St.insert (fromEId, toEId)
    else nonemptyQueues .^^. id
  return ()

pollMsg :: (Co.EndpointId, Co.EndpointId) -> ST GlobalState Ms.Message
pollMsg (fromEId, toEId) = do
  msg <- queues . ix fromEId . ix toEId .^^* U.poll
  queueLength <- queues . ix fromEId . ix toEId .^^^* Sq.length
  if queueLength == 0
    then nonemptyQueues .^^. St.delete (fromEId, toEId)
    else nonemptyQueues .^^. id
  return msg

runIAction :: Co.EndpointId -> Ac.InputAction -> ST GlobalState ()
runIAction eId iAction = do
  (_, msgsO) <- runT (slaveState . ix eId) (TIH.handleInputAction iAction)
  Mo.forM_ msgsO $ \msgO -> do
    case msgO of
      Ac.Send toEIds msg -> Mo.forM_ toEIds $ \toEId -> addMsg msg (eId, toEId)
      Ac.RetryOutput counterValue -> do
        currentTime <- getT $ clocks . ix eId
        asyncQueues . ix eId .^^.* U.push (Ac.RetryInput counterValue, currentTime + 100)
        return ()
      _ -> return ()

deliverMessage :: (Co.EndpointId, Co.EndpointId) -> ST GlobalState ()
deliverMessage (fromEId, toEId) = do
  msg <- pollMsg (fromEId, toEId)
  slaveEIds' <- getL $ slaveEIds
  if Li.elem toEId slaveEIds'
    then runIAction toEId $ Ac.Receive fromEId msg
    else return ()

-- Simulate one millisecond of execution. This involves incrementing the slave's
-- clocks, handling any async tasks whose time has come to execute, and exchanging
-- `numMessages` number of messages.
simulateOnce :: Int -> ST GlobalState ()
simulateOnce numMessages = do
  -- increment each slave's clocks and run async tasks that are now ready
  eIds <- getL slaveEIds
  Mo.forM_ eIds $ \eId -> do
    randPercent :: Int <- rand .^^ Rn.randomR (1, 100)
    if randPercent <= 95
      then do
        clockVal <- clocks . ix eId .^^.* (+1)
        asyncQueue <- getT $ asyncQueues . ix eId
        let (readyActions, remainder) = asyncQueue & Sq.spanl (\(_, time) -> time == clockVal)
        asyncQueues . ix eId .^^.* \_ -> remainder
        Mo.forM_ readyActions $ \(iAction, _) -> runIAction eId iAction
      else return ()
  -- Deliver `numMessages` messages
  Mo.forM_ [1..numMessages] $ \_ -> do
    length <- nonemptyQueues .^^^ St.size
    if length == 0
      then return ()
      else do
        randQueueIndex <- rand .^^ Rn.randomR (0, length - 1)
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
      msg = Ms.ClientRequest $ CM.WriteRequest "uid" "d" "t" ("key " ++ show cliengMsgId) ("value " ++ show cliengMsgId) 1
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
  let paxosLogs = g ^. slaveState & Mp.toList & map (^. _2 . TS.multiPaxosInstance.MP.paxosLog)
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

data TestPaxosLog = TestPaxosLog {
  _plog :: Mp.Map PM.IndexT PM.PaxosLogEntry,
  _nextIdx :: PM.IndexT
}

makeLenses ''TestPaxosLog

type TestPaxosLogs = Mp.Map Co.PaxosId TestPaxosLog

addMsgs
 :: Co.PaxosId
 -> PM.IndexT
 -> Mp.Map PM.IndexT PM.PaxosLogEntry
 -> [TrM.TraceMessage]
 -> (PM.IndexT, [TrM.TraceMessage])
addMsgs paxosId i m msgs =
  case Mp.lookup i m of
    Just v -> addMsgs paxosId (i + 1) m (TrM.PaxosInsertion paxosId i v : msgs)
    Nothing -> (i, msgs)

-- This functions restructures the messages so that PaxosInsertions occur in
-- order of their index, where the occur as early as they are first seen. This function
-- also checks if the PaxosLogInsertion are consistent (i.e. the entry for a PaxosLogInsertion
-- at a given PaxosId and Index are the same).
refineTrace :: [TrM.TraceMessage] -> Either Co.ErrorMsg [TrM.TraceMessage]
refineTrace msgs =
    let paxosLogsE = U.s31 Mo.foldM (Mp.empty, []) msgs $
          \(paxosLogs, modMsgs) msg ->
            case msg of
              TrM.PaxosInsertion paxosId index entry ->
                case paxosLogs ^. at paxosId of
                  Just paxosLog ->
                    case paxosLog ^. plog . at index of
                      Just entry' ->
                        if entry' == entry
                          then Right (paxosLogs, modMsgs)
                          else Left $ "A PaxosLog entry mismatch occurred at: " ++
                                      "PaxosId = " ++ show paxosId ++ ", Entry: " ++ show entry
                      _ ->
                        let plog' = paxosLog ^. plog & at index ?~ entry
                            (nextIdx', modMsgs') = addMsgs paxosId (paxosLog ^. nextIdx) plog' modMsgs
                            paxosLog' = TestPaxosLog plog' nextIdx'
                            paxosLogs' = paxosLogs & at paxosId ?~ paxosLog'
                        in Right (paxosLogs', modMsgs')
                  Nothing ->
                    let plog' = Mp.empty & at index ?~ entry
                        (nextIdx', modMsgs') = addMsgs paxosId 0 plog' modMsgs
                        paxosLog' = TestPaxosLog plog' nextIdx'
                        paxosLogs' = paxosLogs & at paxosId ?~ paxosLog'
                    in Right (paxosLogs', modMsgs')
              _ -> Right (paxosLogs, (msg:modMsgs))
    in case paxosLogsE of
      Right (paxosLogs, modMsgs) -> Right modMsgs
      Left errMsg -> Left errMsg

type RequestMap = Mp.Map Co.RequestId CM.ClientRequest
type Tables = Mp.Map (Co.DatabaseId, Co.TableId) IMS.MultiVersionKVStore

--checkResponses :: [TrM.TraceMessage] -> IO ()
--checkResponses msgs = do
--  let res = U.s31 Mo.foldM (Mp.empty, Mp.empty) msgs $
--        \(requestMap, tables) msg ->
--          case msg of
--            TrM.PaxosInsertion _ index entry ->
--              case of
--                TrM.Tablet ->
--                  case entry of
--                    PM.
--                TrM.Slave -> error "Not handling Slaves yet."
--


-- This list of trace messages includes all events across all slaves.
verifyTrace :: [TrM.TraceMessage] -> IO ()
verifyTrace msgs = do
  -- First, we restructure the messages so that PaxosInsertions happen in order of
  -- ascending index, ensuring that the entries are all consistent.
  let paxosLogsE = refineTrace msgs

  -- Next, we construct the mvkvs and ensure that the responses made to each client
  -- request are correct (at the time the responses are issued).
  case paxosLogsE of
    Right (msgs) -> pPrintNoColor $ reverse msgs
    Left errMsg -> print $ "Failed!: " ++ errMsg

driveTest :: Int -> ST GlobalState () -> IO ()
driveTest testNum test = do
  let g = createGlobalState 0 5 1
      (_, (oActions, traceMsgs, g')) = runST test g
  -- We must reverse traceMsgs since that's created in reverse order,
  -- with the most recent message first
  verifyTrace $ reverse traceMsgs
--  pPrintNoColor traceMsgs
  Mo.forM_ (reverse oActions) $ \action ->
    case action of
      Ac.Print message -> do
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
