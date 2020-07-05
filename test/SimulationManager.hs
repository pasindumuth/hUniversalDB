{-# LANGUAGE ScopedTypeVariables #-}
module SimulationManager where

import qualified Control.Monad as Mo
import qualified Data.List as Li
import qualified Data.Map as Mp
import qualified Data.Set as St
import qualified Data.Sequence as Sq
import qualified System.Random as Rn

import qualified Infra.Utils as U
import qualified Proto.Actions.Actions as Ac
import qualified Proto.Common as Co
import qualified Proto.Messages as Ms
import qualified Proto.Messages.ClientRequests as CRq
import qualified Slave.Env as En
import qualified Slave.SlaveInputHandler as SIH
import qualified Slave.SlaveState as SS
import qualified Slave.Tablet.TabletInputHandler as TIH
import qualified Slave.Tablet.TabletState as TS
import qualified TestState as Tt
import Infra.Lens
import Infra.State

mkSlaveEId i = "s" ++ show i
mkClientEId i = "c" ++ show i

createTestState :: Int -> Int -> Int -> Tt.TestState
createTestState seed numSlaves numClients =
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
              g = SS.constructor "" (Rn.mkStdGen r) slaveEIds
          in ((eId, g):slaves, rand')
      slaveState = Mp.fromList slaves
      tabletStates = Mp.fromList $ U.for slaveEIds $ \eId -> (eId, Mp.empty)
      asyncQueues = Mp.fromList $ U.for slaveEIds $ \eId -> (eId, Sq.empty)
      tabletAsyncQueues = Mp.fromList $ U.for slaveEIds $ \eId -> (eId, Mp.empty)
      clocks = Mp.fromList $ U.for slaveEIds $ \eId -> (eId, 0)
  in Tt.TestState {
      Tt._slaveEIds = slaveEIds,
      Tt._clientEIds = clientEIds,
      Tt._queues = queues,
      Tt._nonemptyQueues = nonemptyQueues,
      Tt._slaveState = slaveState,
      Tt._tabletStates = tabletStates,
      Tt._rand = rand',
      Tt._slaveAsyncQueues = asyncQueues,
      Tt._tabletAsyncQueues = tabletAsyncQueues,
      Tt._clocks = clocks,
      Tt._nextUid = 0
    }

addMsg :: Ms.Message -> (Co.EndpointId, Co.EndpointId) -> ST Tt.TestState ()
addMsg msg (fromEId, toEId) = do
  queue <- Tt.queues . ix fromEId . ix toEId .^^.* U.push msg
  if Sq.length queue == 1
    then Tt.nonemptyQueues .^^. St.insert (fromEId, toEId)
    else Tt.nonemptyQueues .^^. id
  return ()

pollMsg :: (Co.EndpointId, Co.EndpointId) -> ST Tt.TestState Ms.Message
pollMsg (fromEId, toEId) = do
  msg <- Tt.queues . ix fromEId . ix toEId .^^* U.poll
  queueLength <- Tt.queues . ix fromEId . ix toEId .^^^* Sq.length
  if queueLength == 0
    then Tt.nonemptyQueues .^^. St.delete (fromEId, toEId)
    else Tt.nonemptyQueues .^^. id
  return msg

runTabletIAction
  :: Co.EndpointId
  -> Co.KeySpaceRange
  -> Ac.TabletInputAction
  -> ST Tt.TestState ()
runTabletIAction eId range iAction = do
  (_, msgsO) <- runT (Tt.tabletStates . ix eId . ix range) (TIH.handleInputAction iAction)
  Mo.forM_ msgsO $ \msgO -> do
    case msgO of
      Ac.Send toEIds msg -> Mo.forM_ toEIds $ \toEId -> addMsg msg (eId, toEId)
      Ac.RetryOutput counterValue -> do
        currentTime <- getT $ Tt.clocks . ix eId
        Tt.tabletAsyncQueues . ix eId . ix range .^^.* U.push (Ac.TabletRetryInput counterValue, currentTime + 100)
        return ()
      _ -> return ()

runIAction :: Co.EndpointId -> Ac.InputAction -> ST Tt.TestState ()
runIAction eId iAction = do
  (_, msgsO) <- runT (Tt.slaveState . ix eId) (SIH.handleInputAction iAction)
  Mo.forM_ msgsO $ \msgO -> do
    case msgO of
      Ac.Send toEIds msg -> Mo.forM_ toEIds $ \toEId -> addMsg msg (eId, toEId)
      Ac.RetryOutput counterValue -> do
        currentTime <- getT $ Tt.clocks . ix eId
        Tt.slaveAsyncQueues . ix eId .^^.* U.push (Ac.RetryInput counterValue, currentTime + 100)
        return ()
      Ac.Slave_CreateTablet range -> do
        r <- Tt.slaveState . ix eId . SS.env . En.rand .^^* Rn.random
        slaveEIds <- getL $ Tt.slaveEIds
        let tabletState = TS.constructor (show range) (Rn.mkStdGen r) slaveEIds range
        Tt.tabletStates . ix eId .^^.* Mp.insert range tabletState
        return ()
      Ac.TabletForward range clientEId tabletMsg -> do
        runTabletIAction eId range (Ac.TabletReceive clientEId tabletMsg)
      _ -> return ()

deliverMessage :: (Co.EndpointId, Co.EndpointId) -> ST Tt.TestState ()
deliverMessage (fromEId, toEId) = do
  msg <- pollMsg (fromEId, toEId)
  slaveEIds' <- getL $ Tt.slaveEIds
  if Li.elem toEId slaveEIds'
    then runIAction toEId $ Ac.Receive fromEId msg
    else return ()

-- Simulate one millisecond of execution. This involves incrementing the slave's
-- clocks, handling any async tasks whose time has come to execute, and exchanging
-- `numMessages` number of messages.
simulateOnce :: Int -> ST Tt.TestState ()
simulateOnce numMessages = do
  -- increment each slave's clocks and run async tasks that are now ready
  eIds <- getL Tt.slaveEIds
  Mo.forM_ eIds $ \eId -> do
    randPercent :: Int <- Tt.rand .^^ Rn.randomR (1, 100)
    if randPercent <= 95
      then do
        -- Increment clock
        clockVal <- Tt.clocks . ix eId .^^.* (+1)
        -- Process slave async messages
        asyncQueue <- getT $ Tt.slaveAsyncQueues . ix eId
        let (readyActions, remainder) = asyncQueue & Sq.spanl (\(_, time) -> time == clockVal)
        Tt.slaveAsyncQueues . ix eId .^^.* \_ -> remainder
        Mo.forM_ readyActions $ \(iAction, _) -> runIAction eId iAction
        -- Process tablet async messages
        queuesRanges <- Tt.tabletAsyncQueues . ix eId .^^^* Mp.toList
        Mo.forM_ queuesRanges $ \(range, asyncQueue) -> do
          let (readyActions, remainder) = asyncQueue & Sq.spanl (\(_, time) -> time == clockVal)
          Tt.tabletAsyncQueues . ix eId . ix range .^^.* \_ -> remainder
          Mo.forM_ readyActions $ \(iAction, _) -> runTabletIAction eId range iAction
      else return ()
  -- Deliver `numMessages` messages
  Mo.forM_ [1..numMessages] $ \_ -> do
    length <- Tt.nonemptyQueues .^^^ St.size
    if length == 0
      then return ()
      else do
        randQueueIndex <- Tt.rand .^^ Rn.randomR (0, length - 1)
        queueId <- Tt.nonemptyQueues .^^^  St.elemAt randQueueIndex
        deliverMessage queueId

-- Simulate `n` milliseconds of execution
simulateN :: Int -> ST Tt.TestState ()
simulateN n = do
  numChans <- lp2 (Tt.slaveEIds, Tt.clientEIds) .^^^ \(s, c) -> (length s) + (length c)
  Mo.forM_ [1..n] $ \_ -> simulateOnce (numChans * (numChans + 1) `div` 2)

-- Simulate execution until there are no more messages in any channel
-- or any asyncQueue.
simulateAll :: ST Tt.TestState ()
simulateAll = do
  numChans <- lp2 (Tt.slaveEIds, Tt.clientEIds) .^^^ \(s, c) -> (length s) + (length c)
  let simulate =
        do
          length <- Tt.nonemptyQueues .^^^ St.size
          anySlaveTasks <- Tt.slaveAsyncQueues .^^^ any (\q -> Sq.length q > 0)
          anyTabletTasks <- Tt.tabletAsyncQueues .^^^ any (\tablets -> tablets & any (\q -> Sq.length q > 0))
          if length > 0 || anySlaveTasks || anyTabletTasks
            then do
              simulateOnce (numChans * (numChans + 1) `div` 2)
              simulate
            else return ()
  simulate

addClientMsg
  :: Int
  -> CRq.RequestPayload
  -> ST Tt.TestState ()
addClientMsg slaveId payload = do
  uid <- Tt.nextUid .^^. (+1)
  let (clientEId, slaveEId) = (mkClientEId 0, mkSlaveEId slaveId)
      msg = Ms.ClientRequest
              (CRq.ClientRequest
                (CRq.RequestMeta (show uid))
                payload)
  addMsg msg (clientEId, slaveEId)
  return ()
