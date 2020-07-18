{-# LANGUAGE ScopedTypeVariables #-}

module SimulationManager (
  createTestState,
  dropMessages,
  simulateN,
  simulateAll,
  addClientMsg,
  mkMasterEId,
  mkSlaveEId,
  mkClientEId,
) where

import qualified Control.Monad as Mo
import qualified Data.Default as Df
import qualified Data.List as Li
import qualified Data.Map as Mp
import qualified Data.Set as St
import qualified Data.Sequence as Sq
import qualified System.Random as Rn

import qualified Infra.Utils as U
import qualified Master.MasterInputHandler as MIH
import qualified Master.MasterState as MS
import qualified Proto.Actions.MasterActions as MAc
import qualified Proto.Actions.SlaveActions as SAc
import qualified Proto.Actions.TabletActions as TAc
import qualified Proto.Common as Co
import qualified Proto.Messages as Ms
import qualified Proto.Messages.ClientRequests as CRq
import qualified Proto.Messages.ClientResponses as CRs
import qualified Slave.Env as En
import qualified Slave.SlaveInputHandler as SIH
import qualified Slave.SlaveState as SS
import qualified Slave.Tablet.TabletInputHandler as TIH
import qualified Slave.Tablet.TabletState as TS
import qualified ClientState as CS
import qualified RequestStats as RS
import qualified TestState as Tt
import Infra.Lens
import Infra.State

mkMasterEId i = "m" ++ show i
mkSlaveEId i = "s" ++ show i
mkClientEId i = "c" ++ show i

createTestState :: Int -> Int -> Int -> Int -> Tt.TestState
createTestState seed numMasters numSlaves numClients =
  let masterEIds = U.for [0..(numMasters - 1)] $ mkMasterEId
      slaveEIds = U.for [0..(numSlaves - 1)] $ mkSlaveEId
      clientEIds = U.for [0..(numClients - 1)] $ mkClientEId
      eIds = masterEIds ++ slaveEIds ++ clientEIds
      queues = Mp.fromList $ U.for eIds $ \eId1 ->
        (eId1, Mp.fromList $ U.for eIds $ \eId2 ->
        (eId2, Sq.empty))
      nonemptyQueues = St.empty
      rand = Rn.mkStdGen seed
      (masters, rand') = U.s31 foldl ([], rand) masterEIds $
        \(masters, rand) eId ->
          let (r, rand')  = Rn.random rand
              g = MS.constructor "master" (Rn.mkStdGen r) masterEIds slaveEIds
          in ((eId, g):masters, rand')
      masterState = Mp.fromList masters
      (slaves, rand'') = U.s31 foldl ([], rand') slaveEIds $
        \(slaves, rand) eId ->
          let (r, rand')  = Rn.random rand
              g = SS.constructor "" (Rn.mkStdGen r) slaveEIds
          in ((eId, g):slaves, rand')
      slaveState = Mp.fromList slaves
      tabletStates = Mp.fromList $ U.for slaveEIds $ \eId -> (eId, Mp.empty)
      masterAsyncQueues = Mp.fromList $ U.for masterEIds $ \eId -> (eId, Sq.empty)
      slaveAsyncQueues = Mp.fromList $ U.for slaveEIds $ \eId -> (eId, Sq.empty)
      tabletAsyncQueues = Mp.fromList $ U.for slaveEIds $ \eId -> (eId, Mp.empty)
      clocks = Mp.fromList $ U.for (masterEIds ++ slaveEIds) $ \eId -> (eId, 0)
      (clients, rand''') = U.s31 foldl ([], rand'') clientEIds $
        \(clients, rand) eId ->
          let (r, rand')  = Rn.random rand
              client = CS.ClientState St.empty slaveEIds masterEIds (Rn.mkStdGen r)
          in ((eId, client):clients, rand')
      clientState = Mp.fromList clients
  in Tt.TestState {
      Tt._rand = rand''',
      Tt._masterEIds = masterEIds,
      Tt._slaveEIds = slaveEIds,
      Tt._clientEIds = clientEIds,
      Tt._queues = queues,
      Tt._nonemptyQueues = nonemptyQueues,
      Tt._masterState = masterState,
      Tt._slaveState = slaveState,
      Tt._tabletStates = tabletStates,
      Tt._masterAsyncQueues = masterAsyncQueues,
      Tt._slaveAsyncQueues = slaveAsyncQueues,
      Tt._tabletAsyncQueues = tabletAsyncQueues,
      Tt._clocks = clocks,
      Tt._clientState = clientState,
      Tt._nextUid = 0,
      Tt._trueTimestamp = 0,
      Tt._requestStats = Df.def
    }

addMsg :: Ms.Message -> (Co.EndpointId, Co.EndpointId) -> STS Tt.TestState ()
addMsg msg (fromEId, toEId) = do
  queue <- Tt.queues . ix fromEId . ix toEId .^^.* U.push msg
  if Sq.length queue == 1
    then Tt.nonemptyQueues .^^. St.insert (fromEId, toEId)
    else Tt.nonemptyQueues .^^. id
  return ()

pollMsg :: (Co.EndpointId, Co.EndpointId) -> STS Tt.TestState Ms.Message
pollMsg (fromEId, toEId) = do
  msg <- Tt.queues . ix fromEId . ix toEId .^^* U.poll
  queueLength <- Tt.queues . ix fromEId . ix toEId .^^^* Sq.length
  if queueLength == 0
    then Tt.nonemptyQueues .^^. St.delete (fromEId, toEId)
    else Tt.nonemptyQueues .^^. id
  return msg

runMasterIAction :: Co.EndpointId -> MAc.InputAction -> STS Tt.TestState ()
runMasterIAction eId iAction = do
  (_, msgsO) <- runT (Tt.masterState . ix eId) (MIH.handleInputAction iAction)
  Mo.forM_ msgsO $ \msgO -> do
    case msgO of
      MAc.Send toEIds msg -> Mo.forM_ toEIds $ \toEId -> addMsg msg (eId, toEId)
      MAc.RetryOutput counterValue delay -> do
        currentTime <- getT $ Tt.clocks . ix eId
        Tt.masterAsyncQueues . ix eId .^^.* U.push (MAc.RetryInput counterValue, currentTime + delay)
        return ()
      MAc.PerformOutput uid delay -> do
        currentTime <- getT $ Tt.clocks . ix eId
        Tt.masterAsyncQueues . ix eId .^^.* U.push (MAc.PerformInput uid, currentTime + delay)
        return ()
      MAc.Print _ -> return ()

runSlaveIAction :: Co.EndpointId -> SAc.InputAction -> STS Tt.TestState ()
runSlaveIAction eId iAction = do
  (_, msgsO) <- runT (Tt.slaveState . ix eId) (SIH.handleInputAction iAction)
  Mo.forM_ msgsO $ \msgO -> do
    case msgO of
      SAc.Send toEIds msg -> Mo.forM_ toEIds $ \toEId -> addMsg msg (eId, toEId)
      SAc.RetryOutput counterValue delay -> do
        currentTime <- getT $ Tt.clocks . ix eId
        Tt.slaveAsyncQueues . ix eId .^^.* U.push (SAc.RetryInput counterValue, currentTime + delay)
        return ()
      SAc.Slave_CreateTablet ranges' -> do
        ranges <- getT $ Tt.tabletStates . ix eId
        Mo.forM_ ranges' $ \range -> do
          if Mp.member range ranges
            then return ()
            else do
              r <- Tt.slaveState . ix eId . SS.env . En.rand .^^* Rn.random
              slaveEIds <- getL $ Tt.slaveEIds
              let tabletState = TS.constructor (show range) (Rn.mkStdGen r) slaveEIds range
              Tt.tabletStates . ix eId .^^.* Mp.insert range tabletState
              Tt.tabletAsyncQueues . ix eId .^^.* Mp.insert range Sq.empty
              return ()
      SAc.TabletForward range clientEId tabletMsg -> do
        runTabletIAction eId range (TAc.Receive clientEId tabletMsg)
      SAc.Print _ -> return ()

runTabletIAction
  :: Co.EndpointId
  -> Co.KeySpaceRange
  -> TAc.InputAction
  -> STS Tt.TestState ()
runTabletIAction eId range iAction = do
  (_, msgsO) <- runT (Tt.tabletStates . ix eId . ix range) (TIH.handleInputAction iAction)
  Mo.forM_ msgsO $ \msgO -> do
    case msgO of
      TAc.Send toEIds msg -> Mo.forM_ toEIds $ \toEId -> addMsg msg (eId, toEId)
      TAc.RetryOutput counterValue delay -> do
        currentTime <- getT $ Tt.clocks . ix eId
        Tt.tabletAsyncQueues . ix eId . ix range .^^.* U.push (TAc.RetryInput counterValue, currentTime + delay)
        return ()
      TAc.Print _ -> return ()

deliverMessage :: (Co.EndpointId, Co.EndpointId) -> STS Tt.TestState ()
deliverMessage (fromEId, toEId) = do
  msg <- pollMsg (fromEId, toEId)
  masterEIds' <- getL $ Tt.masterEIds
  slaveEIds' <- getL $ Tt.slaveEIds
  clientEIds' <- getL $ Tt.clientEIds
  let route
        | Li.elem toEId masterEIds' =
            runMasterIAction toEId $ MAc.Receive fromEId msg
        | Li.elem toEId slaveEIds' =
            runSlaveIAction toEId $ SAc.Receive fromEId msg
        | Li.elem toEId clientEIds' = do
            case msg of
              Ms.ClientResponse response -> do
                Tt.requestStats .^ RS.recordResponse (response ^. CRs.payload)
                Tt.clientState . ix toEId .^* CS.handleResponse response
              _ -> U.caseError -- Any messages coming to a clientEId must be a response.
        | otherwise = U.caseError
  route

dropMessages :: Int -> STS Tt.TestState ()
dropMessages numMessages = do
  Mo.forM_ [1..numMessages] $ \_ -> do
    length <- Tt.nonemptyQueues .^^^ St.size
    if length == 0
      then return ()
      else do
        randQueueIndex <- Tt.rand .^^ Rn.randomR (0, length - 1)
        queueId <- Tt.nonemptyQueues .^^^  St.elemAt randQueueIndex
        pollMsg queueId
        return ()

skewProb = 95

-- Simulate one millisecond of execution. This involves incrementing the slave's
-- clocks, handling any async tasks whose time has come to execute, and exchanging
-- `numMessages` number of messages.
simulateOnce :: Int -> STS Tt.TestState ()
simulateOnce numMessages = do
  -- increment each slave's clocks and run async tasks that are now ready
  randPercent :: Int <- Tt.rand .^^ Rn.randomR (1, 100)
  if randPercent <= skewProb
    then do
      -- We have to put this behind a randPercentage with the the same
      -- probability of success as the Tt.clocks to prevent the trueTime from
      -- pulling away.
      Tt.trueTimestamp .^^. (+1)
      return ()
    else return ()
  masterEIds <- getL Tt.masterEIds
  Mo.forM_ masterEIds $ \eId -> do
    randPercent :: Int <- Tt.rand .^^ Rn.randomR (1, 100)
    if randPercent <= skewProb
      then do
        -- Increment clock
        clockVal <- Tt.clocks . ix eId .^^.* (+1)
        -- Process master async messages
        asyncQueue <- getT $ Tt.masterAsyncQueues . ix eId
        let (readyActions, remainder) = asyncQueue & Sq.spanl (\(_, time) -> time == clockVal)
        Tt.masterAsyncQueues . ix eId .^^.* \_ -> remainder
        Mo.forM_ readyActions $ \(iAction, _) -> runMasterIAction eId iAction
      else return ()
  slaveEIds <- getL Tt.slaveEIds
  Mo.forM_ slaveEIds $ \eId -> do
    randPercent :: Int <- Tt.rand .^^ Rn.randomR (1, 100)
    if randPercent <= skewProb
      then do
        -- Increment clock
        clockVal <- Tt.clocks . ix eId .^^.* (+1)
        -- Process slave async messages
        asyncQueue <- getT $ Tt.slaveAsyncQueues . ix eId
        let (readyActions, remainder) = asyncQueue & Sq.spanl (\(_, time) -> time == clockVal)
        Tt.slaveAsyncQueues . ix eId .^^.* \_ -> remainder
        Mo.forM_ readyActions $ \(iAction, _) -> runSlaveIAction eId iAction
        -- Process tablet async messages
        queuesRanges <- Tt.tabletAsyncQueues . ix eId .^^^* Mp.toList
        Mo.forM_ queuesRanges $ \(range, asyncQueue) -> do
          let (readyActions, remainder) = asyncQueue & Sq.spanl (\(_, time) -> time == clockVal)
          Tt.tabletAsyncQueues . ix eId . ix range .^^.* \_ -> remainder
          Mo.forM_ readyActions $ \(iAction, _) -> runTabletIAction eId range iAction
      else return ()
  -- Deliver messages
  --
  -- Here, we are delivering approximately as many messages as there are currently
  -- in the channels. We select which channels to deliver by first building a distribution,
  -- lenDis, of the number of messages that are in every nonemptyQueue. Then, we select which
  -- queue to deliver from according to this distribution.
  --
  -- We use this scheme to model the fact that channels have about a 1 millisecond latency,
  -- but much higher bandwidth. Thus, between two cycles of simulateOnce, all messages in a channel
  -- should have been delivered.
  nonemptyQueues <- getL $ Tt.nonemptyQueues
  lenDist <-
    U.s31 Mo.foldM [] nonemptyQueues $ \lenDist queueId -> do
      let (fromEId, toEId) = queueId
      queue <- getT $ Tt.queues . ix fromEId . ix toEId
      return $ (queueId, Sq.length queue):lenDist

  let numMessages = foldl (\s (_, a) -> s + a) 0 lenDist
  Mo.forM_ [1..numMessages] $ \_ -> do
    r :: Int <- Tt.rand .^^ Rn.randomR (0, numMessages - 1)
    let loop left ((queueId, distVal):xs)
          | distVal <= left = loop (left - distVal) xs
          | otherwise = queueId
        loop left [] = U.caseError
        queueId = loop r lenDist
    nonemptyQueues <- getL $ Tt.nonemptyQueues
    if St.member queueId nonemptyQueues
      then deliverMessage queueId
      else return ()

-- Simulate `n` milliseconds of execution
simulateN :: Int -> STS Tt.TestState ()
simulateN n = do
  numChans <- lp3 (Tt.slaveEIds, Tt.clientEIds, Tt.masterEIds) .^^^ \(s, c, m) -> (length s) + (length c) + (length m)
  Mo.forM_ [1..n] $ \_ -> simulateOnce (numChans * (numChans + 1) `div` 2)

-- Simulate execution until there are no more messages in any channel
-- or any asyncQueue.
simulateAll :: STS Tt.TestState ()
simulateAll = do
  numChans <- lp3 (Tt.slaveEIds, Tt.clientEIds, Tt.masterEIds) .^^^ \(s, c, m) -> (length s) + (length c) + (length m)
  let simulate =
        do
          length <- Tt.nonemptyQueues .^^^ St.size
          anyMasterTasks <- Tt.masterAsyncQueues .^^^ any (\q -> Sq.length q > 0)
          anySlaveTasks <- Tt.slaveAsyncQueues .^^^ any (\q -> Sq.length q > 0)
          anyTabletTasks <- Tt.tabletAsyncQueues .^^^ any (\tablets -> tablets & any (\q -> Sq.length q > 0))
          if length > 0 || anyMasterTasks || anySlaveTasks || anyTabletTasks
            then do
              simulateOnce (numChans * (numChans + 1) `div` 2)
              simulate
            else return ()
  simulate

addClientMsg
  :: Co.EndpointId
  -> Co.EndpointId
  -> CRq.Payload
  -> STS Tt.TestState ()
addClientMsg fromEId toEId payload = do
  uid <- Tt.nextUid .^^. (+1)
  let msg = Ms.ClientRequest
              (CRq.ClientRequest
                (CRq.Meta (show uid))
                payload)
  addMsg msg (fromEId, toEId)
  Tt.requestStats .^ RS.recordRequest payload
