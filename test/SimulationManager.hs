{-# LANGUAGE ScopedTypeVariables #-}

module SimulationManager (
  createTestState,
  dropMessages,
  simulate1ms,
  simulateNms,
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

mkMasterEId i = Co.EndpointId $ "m" ++ show i
mkSlaveGroupId i = Co.SlaveGroupId $ "sg" ++ show i
mkSlaveEId i j = Co.EndpointId $ "sg" ++ show i ++ "sl" ++ show j
mkClientEId i = Co.EndpointId $ "c" ++ show i

numPerGroup :: Int = 5

createTestState :: Int -> Int -> Int -> Tt.TestState
createTestState seed numSlaveGroups numClients =
  let masterEIds = U.map
        [0..(numPerGroup - 1)] $ mkMasterEId
      slaveGroupEIds = Mp.fromList $ U.map
        [0..(numSlaveGroups - 1)] $ \i -> (mkSlaveGroupId i, U.map
        [0..(numPerGroup - 1)] $ \j -> mkSlaveEId i j)
      allSlaveEIds = Mp.foldl (\slaveEIds eIds -> slaveEIds ++ eIds) [] slaveGroupEIds
      clientEIds = U.map
        [0..(numClients - 1)] $ mkClientEId
      eIds = masterEIds ++ allSlaveEIds ++ clientEIds
      queues = Mp.fromList $ U.map eIds $ \eId1 ->
        (eId1, Mp.fromList $ U.map eIds $ \eId2 ->
        (eId2, Sq.empty))
      nonemptyQueues = St.empty
      rand = Rn.mkStdGen seed
      (masterPId, rand') = U.mkUID rand & _1 %~ Co.PaxosId
      (masters, rand'') = U.s31 foldl ([], rand') masterEIds $
        \(masters, rand) eId ->
          let (r, rand')  = Rn.random rand
              g = MS.constructor masterPId (Rn.mkStdGen r) masterEIds slaveGroupEIds
          in ((eId, g):masters, rand')
      masterState = Mp.fromList masters
      (slaves, rand''') = U.s31 Mp.foldlWithKey ([], rand'') slaveGroupEIds $
        \(slaves, rand) slaveGroupId slaveEIds ->
          let (slavePId, rand') = U.mkUID rand & _1 %~ Co.PaxosId
          in U.s31 foldl (slaves, rand') slaveEIds $
            \(slaves, rand) eId ->
              let (r, rand')  = Rn.random rand
                  g = SS.constructor slaveGroupId slavePId (Rn.mkStdGen r) slaveEIds
              in ((eId, g):slaves, rand')
      slaveState = Mp.fromList slaves
      tabletStates = Mp.fromList $ U.map allSlaveEIds $ \eId -> (eId, Mp.empty)
      masterAsyncQueues = Mp.fromList $ U.map masterEIds $ \eId -> (eId, Sq.empty)
      slaveAsyncQueues = Mp.fromList $ U.map allSlaveEIds $ \eId -> (eId, Sq.empty)
      tabletAsyncQueues = Mp.fromList $ U.map allSlaveEIds $ \eId -> (eId, Mp.empty)
      clocks = Mp.fromList $ U.map (masterEIds ++ allSlaveEIds) $ \eId -> (eId, 0)
      (clients, rand'''') = U.s31 foldl ([], rand''') clientEIds $
        \(clients, rand) eId ->
          let (r, rand')  = Rn.random rand
              slaveGroups = Mp.map (\slaveEIds -> (slaveEIds, St.empty)) slaveGroupEIds 
              client = CS.ClientState slaveGroups masterEIds (Rn.mkStdGen r)
          in ((eId, client):clients, rand')
      clientState = Mp.fromList clients
  in Tt.TestState {
      Tt._rand = rand'''',
      Tt._masterEIds = masterEIds,
      Tt._slaveGroupEIds = slaveGroupEIds,
      Tt._allSlaveEIds = allSlaveEIds,
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
      Tt._nextInt = 0,
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
      SAc.Slave_CreateTablet requestId rangeTIds -> do
        tabletMap <- getT $ Tt.tabletStates . ix eId
        Mo.forM_ rangeTIds $ \(_, tabletId) -> do
          if Mp.member tabletId tabletMap
            then return ()
            else do
              r <- Tt.slaveState . ix eId . SS.env . En.rand .^^* Rn.random
              slaveGroupId <- getT $ Tt.slaveState . ix eId . SS.slaveGroupId
              slaveEIds <- getT $ Tt.slaveGroupEIds . ix slaveGroupId
              let tabletState = TS.constructor tabletId (Rn.mkStdGen r) slaveEIds
              Tt.tabletStates . ix eId .^^.* Mp.insert tabletId tabletState
              Tt.tabletAsyncQueues . ix eId .^^.* Mp.insert tabletId Sq.empty
              return ()
      SAc.TabletForward tabletId clientEId tabletMsg -> do
        runTabletIAction eId tabletId (TAc.Receive clientEId tabletMsg)
      SAc.Print _ -> return ()

runTabletIAction
  :: Co.EndpointId
  -> Co.TabletId
  -> TAc.InputAction
  -> STS Tt.TestState ()
runTabletIAction eId tabletId iAction = do
  (_, msgsO) <- runT (Tt.tabletStates . ix eId . ix tabletId) (TIH.handleInputAction iAction)
  Mo.forM_ msgsO $ \msgO -> do
    case msgO of
      TAc.Send toEIds msg -> Mo.forM_ toEIds $ \toEId -> addMsg msg (eId, toEId)
      TAc.RetryOutput counterValue delay -> do
        currentTime <- getT $ Tt.clocks . ix eId
        Tt.tabletAsyncQueues . ix eId . ix tabletId .^^.* U.push (TAc.RetryInput counterValue, currentTime + delay)
        return ()
      TAc.Print _ -> return ()

deliverMessage :: (Co.EndpointId, Co.EndpointId) -> STS Tt.TestState ()
deliverMessage (fromEId, toEId) = do
  msg <- pollMsg (fromEId, toEId)
  masterEIds' <- getL $ Tt.masterEIds
  slaveEIds' <- getL $ Tt.allSlaveEIds
  clientEIds' <- getL $ Tt.clientEIds
  let allEIds = masterEIds' ++ slaveEIds' ++ clientEIds'
      route
        | Li.elem toEId masterEIds' =
            runMasterIAction toEId $ MAc.Receive fromEId msg
        | Li.elem toEId slaveEIds' =
            runSlaveIAction toEId $ SAc.Receive fromEId msg
        | Li.elem toEId clientEIds' = do
            case msg of
              Ms.ClientResponse response -> do
                Tt.requestStats .^ RS.recordResponse (response ^. CRs.payload)
                Tt.clientState . ix toEId .^* CS.handleResponse fromEId response
              _ -> error $ "The message " ++ (show msg) ++ " should never be receieved by a client."
        | otherwise = error $ "Can't deliver message; " ++ (show toEId) ++
                              " doesn't present in any existing " ++ " endpoint: " ++ (show allEIds)
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
simulate1ms :: STS Tt.TestState ()
simulate1ms = do
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
  slaveEIds <- getL Tt.allSlaveEIds
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
  -- but much higher bandwidth. Thus, between two cycles of simulate1ms, all messages in a channel
  -- should have been delivered.
  nonemptyQueues <- getL $ Tt.nonemptyQueues
  lenDist <-
    U.foldM [] nonemptyQueues $ \lenDist queueId -> do
      let (fromEId, toEId) = queueId
      queue <- getT $ Tt.queues . ix fromEId . ix toEId
      return $ (queueId, Sq.length queue):lenDist

  let numMessages = foldl (\s (_, a) -> s + a) 0 lenDist
  Mo.forM_ [1..numMessages] $ \_ -> do
    r :: Int <- Tt.rand .^^ Rn.randomR (0, numMessages - 1)
    let loop left ((queueId, distVal):xs)
          | distVal <= left = loop (left - distVal) xs
          | otherwise = queueId
        loop left [] = error $ "Shouldn't get here."
        queueId = loop r lenDist
    nonemptyQueues <- getL $ Tt.nonemptyQueues
    if St.member queueId nonemptyQueues
      then deliverMessage queueId
      else return ()

-- Simulate `n` milliseconds of execution
simulateNms :: Int -> STS Tt.TestState ()
simulateNms n = do
  Mo.forM_ [1..n] $ \_ -> simulate1ms

-- Simulate execution until there are no more messages in any channel
-- or any asyncQueue.
simulateAll :: STS Tt.TestState ()
simulateAll = do
  let simulate =
        do
          length <- Tt.nonemptyQueues .^^^ St.size
          anyMasterTasks <- Tt.masterAsyncQueues .^^^ any (\q -> Sq.length q > 0)
          anySlaveTasks <- Tt.slaveAsyncQueues .^^^ any (\q -> Sq.length q > 0)
          anyTabletTasks <- Tt.tabletAsyncQueues .^^^ any (\tablets -> tablets & any (\q -> Sq.length q > 0))
          if length > 0 || anyMasterTasks || anySlaveTasks || anyTabletTasks
            then do
              simulate1ms
              simulate
            else return ()
  simulate

addClientMsg
  :: Co.EndpointId
  -> Co.EndpointId
  -> CRq.Payload
  -> STS Tt.TestState ()
addClientMsg fromEId toEId payload = do
  int <- Tt.nextInt .^^. (+1)
  let msg = Ms.ClientRequest
              (CRq.ClientRequest
                (CRq.Meta $ Co.RequestId $ show int)
                payload)
  addMsg msg (fromEId, toEId)
  Tt.requestStats .^ RS.recordRequest payload
