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
import qualified Proto.Messages.ClientRequests as CRq
import qualified Proto.Messages.ClientResponses as CRs
import qualified Proto.Messages.PaxosMessages as PM
import qualified Proto.Messages.TabletMessages as TM
import qualified Proto.Messages.TraceMessages as TrM
import qualified Slave.Env as En
import qualified Slave.SlaveInputHandler as SIH
import qualified Slave.SlaveState as SS
import qualified Slave.Tablet.Env as TEn
import qualified Slave.Tablet.Internal_MultiVersionKVStore as IMS
import qualified Slave.Tablet.MultiVersionKVStore as MS
import qualified Slave.Tablet.TabletInputHandler as TIH
import qualified Slave.Tablet.TabletState as TS
import qualified Slave.SlaveInputHandler as SIH
import Infra.Lens
import Infra.State

type Queues = Mp.Map Co.EndpointId (Mp.Map Co.EndpointId (Sq.Seq Ms.Message))
type NonemptyQueues = St.Set (Co.EndpointId, Co.EndpointId)

-- TODO Move this it's own file and isolate the simulation code.
data GlobalState = GlobalState {
  _slaveEIds :: [Co.EndpointId], -- EndpointIds for all slaves in the system
  _clientEIds :: [Co.EndpointId], -- EndpointIds for all client's we use for testing
  -- `queues` contains 2 queues (in for each direction) for every pair of
  -- for both client EndpointIds and slave Endpoints.
  _queues :: Queues,
  -- We use pairs of endpoints as identifiers of a queue. `nonemptyQueues`
  -- contain all queue IDs where the queue is non-empty
  _nonemptyQueues :: NonemptyQueues,
  _slaveState :: Mp.Map Co.EndpointId SS.SlaveState,
  _tabletStates :: Mp.Map Co.EndpointId (Mp.Map Co.KeySpaceRange TS.TabletState),
  _rand :: Rn.StdGen, -- Random Number Generator for simulation

  -- The following are everything related to asynchronous computation
  -- done at a node.
  _asyncQueues :: Mp.Map Co.EndpointId (Sq.Seq (Ac.InputAction, Int)),
  _tabletAsyncQueues :: Mp.Map Co.EndpointId (Mp.Map Co.KeySpaceRange (Sq.Seq (Ac.TabletInputAction, Int))),
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
      queues = Mp.fromList $ U.for eIds $ \eId1 ->
        (eId1, Mp.fromList $ U.for eIds $ \eId2 ->
        (eId2, Sq.empty))
      nonemptyQueues = St.empty
      rand = Rn.mkStdGen seed
      (slaves, rand') = U.s31 foldl ([], rand) slaveEIds $
        \(slaves, rand) eId ->
          let (r, rand')  = Rn.random rand
              -- TODO: Create a constructor for this too and remove the default construction.
              g = Df.def & SS.env . En.rand .~ (Rn.mkStdGen r) & SS.env . En.slaveEIds .~ slaveEIds
          in ((eId, g):slaves, rand')
      slaveState = Mp.fromList slaves
      tabletStates = Mp.fromList $ U.for slaveEIds $ \eId -> (eId, Mp.empty)
      asyncQueues = Mp.fromList $ U.for slaveEIds $ \eId -> (eId, Sq.empty)
      tabletAsyncQueues = Mp.fromList $ U.for slaveEIds $ \eId -> (eId, Mp.empty)
      clocks = Mp.fromList $ U.for slaveEIds $ \eId -> (eId, 0)
  in GlobalState {
      _slaveEIds = slaveEIds,
      _clientEIds = clientEIds,
      _queues = queues,
      _nonemptyQueues = nonemptyQueues,
      _slaveState = slaveState,
      _tabletStates = tabletStates,
      _rand = rand',
      _asyncQueues = asyncQueues,
      _tabletAsyncQueues = tabletAsyncQueues,
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

runTabletIAction
  :: Co.EndpointId
  -> Co.KeySpaceRange
  -> Ac.TabletInputAction
  -> ST GlobalState ()
runTabletIAction eId range iAction = do
  (_, msgsO) <- runT (tabletStates . ix eId . ix range) (TIH.handleInputAction iAction)
  Mo.forM_ msgsO $ \msgO -> do
    case msgO of
      Ac.Send toEIds msg -> Mo.forM_ toEIds $ \toEId -> addMsg msg (eId, toEId)
      Ac.RetryOutput counterValue -> do
        currentTime <- getT $ clocks . ix eId
        tabletAsyncQueues . ix eId . ix range .^^.* U.push (Ac.TabletRetryInput counterValue, currentTime + 100)
        return ()
      _ -> return ()

runIAction :: Co.EndpointId -> Ac.InputAction -> ST GlobalState ()
runIAction eId iAction = do
  (_, msgsO) <- runT (slaveState . ix eId) (SIH.handleInputAction iAction)
  Mo.forM_ msgsO $ \msgO -> do
    case msgO of
      Ac.Send toEIds msg -> Mo.forM_ toEIds $ \toEId -> addMsg msg (eId, toEId)
      Ac.RetryOutput counterValue -> do
        currentTime <- getT $ clocks . ix eId
        asyncQueues . ix eId .^^.* U.push (Ac.RetryInput counterValue, currentTime + 100)
        return ()
      Ac.Slave_CreateTablet range -> do
        r <- slaveState . ix eId . SS.env . En.rand .^^* Rn.random
        slaveEIds <- getL $ slaveEIds
        let tabletState = Df.def & TS.env . TEn.rand .~ (Rn.mkStdGen r)
                                 & TS.env . TEn.slaveEIds .~ slaveEIds
                                 & TS.range .~ range
                                 & TS.multiPaxosInstance . MP.paxosId .~ (show range)
        tabletStates . ix eId .^^.* Mp.insert range tabletState
        return ()
      Ac.TabletForward range clientEId tabletMsg -> do
        runTabletIAction eId range (Ac.TabletReceive clientEId tabletMsg)
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
        -- Increment clock
        clockVal <- clocks . ix eId .^^.* (+1)
        -- Process slave async messages
        asyncQueue <- getT $ asyncQueues . ix eId
        let (readyActions, remainder) = asyncQueue & Sq.spanl (\(_, time) -> time == clockVal)
        asyncQueues . ix eId .^^.* \_ -> remainder
        Mo.forM_ readyActions $ \(iAction, _) -> runIAction eId iAction
        -- Process tablet async messages
        queuesRanges <- tabletAsyncQueues . ix eId .^^^* Mp.toList
        Mo.forM_ queuesRanges $ \(range, asyncQueue) -> do
          let (readyActions, remainder) = asyncQueue & Sq.spanl (\(_, time) -> time == clockVal)
          tabletAsyncQueues . ix eId . ix range .^^.* \_ -> remainder
          Mo.forM_ readyActions $ \(iAction, _) -> runTabletIAction eId range iAction
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
          anySlaveTasks <- asyncQueues .^^^ any (\q -> Sq.length q > 0)
          anyTabletTasks <- tabletAsyncQueues .^^^ any (\tablets -> tablets & any (\q -> Sq.length q > 0))
          if length > 0 || anySlaveTasks || anyTabletTasks
            then do
              simulateOnce (numChans * (numChans + 1) `div` 2)
              simulate
            else return ()
  simulate


defaultDatabaseId = "d"
defaultTableId = "t"

addClientMsg :: String -> Int -> ST GlobalState ()
addClientMsg uid slaveId = do
  let (clientEId, slaveEId) = (mkClientEId 0, mkSlaveEId slaveId)
      msg = Ms.ClientRequest
              (CRq.ClientRequest
                (CRq.RequestMeta uid)
                (CRq.WriteRequest
                  defaultDatabaseId
                  defaultTableId
                  ("key " ++ show uid)
                  ("value " ++ show uid)
                  1))

  addMsg msg (clientEId, slaveEId)
  return ()

-- TODO: This is seriously not good. Do all of this properly. Figure out
-- suitable semantics for createDB in our half-backed dbms,
addCreateDBClientMsg :: String -> Int -> ST GlobalState ()
addCreateDBClientMsg uid slaveId = do
  let (clientEId, slaveEId) = (mkClientEId 0, mkSlaveEId slaveId)
      msg = Ms.ClientRequest
              (CRq.ClientRequest
                (CRq.RequestMeta uid)
                (CRq.CreateDatabase
                  defaultDatabaseId
                  defaultTableId))

  addMsg msg (clientEId, slaveEId)
  return ()

test1 :: ST GlobalState ()
test1 = do
  addCreateDBClientMsg "0" 0; simulateAll
  addClientMsg "1" 1; simulateAll

test2 :: ST GlobalState ()
test2 = do
  addCreateDBClientMsg "0" 0; simulateN 2
  addClientMsg "1" 1; simulateN 2
  addClientMsg "2" 2; simulateN 2
  addClientMsg "3" 3; simulateN 2
  addClientMsg "4" 4; simulateN 2
  addClientMsg "5" 0; simulateN 2

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
      Right (paxosLogs, modMsgs) -> Right $ reverse modMsgs
      Left errMsg -> Left errMsg

type RequestMap = Mp.Map Co.RequestId CRq.RequestPayload
type Tables = Mp.Map (Co.DatabaseId, Co.TableId) IMS.MultiVersionKVStore

-- TODO: We should be able to construct the (databaseId, tabletId)
-- from the Slave messages. We'll do that later when we handle Slave messages.
-- TODO: Implement lenses for client messages, and use them.
checkResponses :: [TrM.TraceMessage] -> Either String ()
checkResponses msgs =
  let tables = Mp.empty & Mp.insert (defaultDatabaseId, defaultTableId) Mp.empty
      genericError response payload = Left $ "Response " ++ show response ++ " is the wrong response to Request " ++ show payload
      testRes = U.s31 Mo.foldM (Mp.empty, tables) msgs $
        \(requestMap, tables) msg ->
          case msg of
            TrM.PaxosInsertion _ _ entry ->
              case entry of
                PM.Tablet entry ->
                  case entry of
                    PM.Read key timestamp ->
                      -- TODO: use the paxosId to figure out which table this entry belongs to.
                      let (_, tables') = tables %^~* (ix (defaultDatabaseId, defaultTableId)) $ MS.read key timestamp
                      in Right (requestMap, tables')
                    PM.Write key value timestamp ->
                      -- TODO: check for the case where the lat would fail due to the lat
                      let (_, tables') = tables %^~* (ix (defaultDatabaseId, defaultTableId)) $ MS.write key value timestamp
                      in Right (requestMap, tables')
                PM.Slave _ ->
                  -- TODO: for now, we ignore the AddRange PaxosLogEntry, since we don't need it yet
                  return (requestMap, tables)
            TrM.ClientRequestReceived request@(CRq.ClientRequest (CRq.RequestMeta requestId) payload) ->
              Right (requestMap & Mp.insert requestId payload, tables)
            TrM.ClientResponseSent response@(CRs.ClientResponse (CRs.ResponseMeta requestId) responsePayload) ->
              case requestMap ^. at requestId of
                Nothing -> Left $ "Response " ++ show response ++ " has no corresponding request."
                Just payload -> do
                  let genericSuccess = Right (requestMap & Mp.delete requestId, tables)
                  case payload of
                    CRq.CreateDatabase databaseId tableId ->
                      case responsePayload of
                        CRs.Success -> genericSuccess
                        _ -> genericError response payload
                    CRq.ReadRequest databaseId tableId key timestamp ->
                      case tables ^. at (databaseId, tableId) of
                        Just table ->
                          case responsePayload of
                            CRs.ReadResponse val | val == (MS.staticRead key timestamp table) -> genericSuccess
                            _ -> genericError response payload
                        Nothing ->
                          case responsePayload of
                            CRs.Error msg | msg == SIH.dbTableDNE -> genericSuccess
                            _ -> genericError response payload
                    CRq.WriteRequest databaseId tableId key value timestamp ->
                      case tables ^. at (databaseId, tableId) of
                        Just table ->
                          case MS.staticReadLat key table of
                            Just lat ->
                              case responsePayload of
                                -- The lat should be equal to timestamp. This is because by this point in
                                -- the trace messages, the write PaxosLogEntry should have been encoutered.
                                -- TODO: it's possible that the `lat` was already here when the WriteRequest was received
                                -- but that we're accidentally returning a `WriteResponse` instead of an error. We must
                                -- figure out how to handle this issue.
                                CRs.WriteResponse | lat == timestamp -> genericSuccess
                                CRs.Error msg | msg == TIH.pastWriteAttempt -> genericSuccess
                                _ -> genericError response payload
                            Nothing -> genericError response payload
                        Nothing ->
                          case responsePayload of
                            CRs.Error msg | msg == SIH.dbTableDNE -> genericSuccess
                            _ -> genericError response payload
  in case testRes of
    Right (requestMap, tables) ->
      if Mp.size requestMap > 0
        then Left $ "The following requests went unanswered: " ++ show requestMap
        else Right ()
    Left errMsg -> Left errMsg

-- This list of trace messages includes all events across all slaves.
verifyTrace :: [TrM.TraceMessage] -> Either String ()
verifyTrace msgs =
  -- First, we restructure the messages so that PaxosInsertions happen in order of
  -- ascending index, ensuring that the entries are all consistent.
  let paxosLogsE = refineTrace msgs
  in case paxosLogsE of
    Left errMsg -> Left errMsg
    Right msgs ->
      -- Next, we construct the mvkvs and ensure that the responses made to each client
      -- request are correct (at the time the responses are issued).
      let responseCheck = checkResponses msgs
      in case responseCheck of
        Left errMsg -> Left errMsg
        Right _ -> Right ()

driveTest :: Int -> ST GlobalState () -> IO ()
driveTest testNum test = do
  let g = createGlobalState 0 5 1
      (_, (oActions, traceMsgs, g')) = runST test g
      -- We must reverse traceMsgs since that's created in reverse order,
      -- with the most recent message first
      testMsg =
        case verifyTrace $ reverse traceMsgs of
          Left errMsg -> "Test " ++ (show testNum) ++ " Failed! Error: " ++ errMsg
          Right _ -> "Test " ++ (show testNum) ++ " Passed! " ++ (show $ Li.length traceMsgs) ++ " trace messages."
  print testMsg
  return ()

testDriver :: IO ()
testDriver = do
  driveTest 1 test1
  driveTest 2 test2

main :: IO ()
main = testDriver
