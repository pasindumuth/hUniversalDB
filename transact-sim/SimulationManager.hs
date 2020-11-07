{-# LANGUAGE ScopedTypeVariables #-}

module SimulationManager where

import qualified Control.Monad as Mo
import qualified Data.List as Li
import qualified Data.Map as Mp
import qualified Data.Set as St
import qualified Data.Sequence as Sq
import qualified System.Random as Rn

import qualified Common.Model.RelationalTablet as RT
import qualified Infra.Utils as U
import qualified TestState as Tt
import qualified Transact.Model.Actions as Ac
import qualified Transact.Model.Common as Co
import qualified Transact.Model.Message as Ms
import qualified Transact.Server.ServerState as SS
import qualified Transact.Server.ServerInputHandler as SIH
import qualified Transact.Tablet.TabletState as TS
import qualified Transact.Tablet.TabletInputHandler as TIH
import Infra.Lens
import Transact.Infra.State

-- | Hard simple coded table schema we will use for all tables.
schema :: RT.Schema
schema = RT.Schema [("key", RT.CT'String)] [("value", RT.CT'Int)]

-- | A simpe utility for making Co.TabletKeyRanges for the specific schema above
mkKeyRange :: Maybe String -> Maybe String -> Co.TabletKeyRange
mkKeyRange startKeyM endKeyM =
  Co.TabletKeyRange (fmap toPrimary startKeyM) (fmap toPrimary endKeyM)
  where
    toPrimary key = RT.PrimaryKey [RT.CV'String key]

-- | Hard coded table partitioning configurations. We only handle 5 servers and
-- only a handful of tables. We don't support SQL DDL yet.
partitionConfig :: Mp.Map Co.EndpointId [Co.TabletShape]
partitionConfig = Mp.fromList [
  (Co.EndpointId "s0", [
    Co.TabletShape (Co.TabletPath "table1") (mkKeyRange Nothing Nothing)]),
  (Co.EndpointId "s1", [
    Co.TabletShape (Co.TabletPath "table2") (mkKeyRange Nothing (Just "j"))]),
  (Co.EndpointId "s2", [
    Co.TabletShape (Co.TabletPath "table2") (mkKeyRange (Just "j") Nothing),
    Co.TabletShape (Co.TabletPath "table3") (mkKeyRange Nothing (Just "d")),
    Co.TabletShape (Co.TabletPath "table4") (mkKeyRange Nothing (Just "k"))]),
  (Co.EndpointId "s3", [
    Co.TabletShape (Co.TabletPath "table3") (mkKeyRange (Just "d") (Just "p"))]),
  (Co.EndpointId "s4", [
    Co.TabletShape (Co.TabletPath "table3") (mkKeyRange (Just "p") Nothing),
    Co.TabletShape (Co.TabletPath "table4") (mkKeyRange (Just "k") Nothing)])]

mkServerEId i = Co.EndpointId $ "s" ++ show i
mkClientEId i = Co.EndpointId $ "c" ++ show i

createTestState :: Int -> Int -> Int -> Tt.TestState
createTestState seed numServers numClients =
  let rand = Rn.mkStdGen seed
      serverEIds = U.map
        [0..(numServers - 1)] $ mkServerEId
      clientEIds = U.map
        [0..(numClients - 1)] $ mkClientEId
      eIds = serverEIds ++ clientEIds
      queues = Mp.fromList $ U.map eIds $ \eId1 ->
        (eId1, Mp.fromList $ U.map eIds $ \eId2 ->
        (eId2, Sq.empty))
      nonemptyQueues = St.empty
      (serverStates, rand'') = U.fold (Mp.empty, rand) serverEIds $
        \(serverStates, rand) eId ->
          let (r, rand')  = Rn.random rand
              shapesWithSchema = U.map (partitionConfig ^?! ix eId) $ (,) schema
              g = SS.constructor (Rn.mkStdGen r) shapesWithSchema
          in (Mp.insert eId g serverStates, rand')
      (tabletStates, rand''') = U.fold (Mp.empty, rand'') serverEIds $
        \(tabletStates, rand) eId ->
          let (tabletStatesForServer, rand'') = U.fold (Mp.empty, rand) (partitionConfig ^?! ix eId) $
                \(tabletStatesForServer, rand) tabletShape ->
                  let (r, rand') = Rn.random rand
                      tabletState = TS.constructor (Rn.mkStdGen r) schema
                  in (Mp.insert tabletShape tabletState tabletStatesForServer, rand')
          in (Mp.insert eId tabletStatesForServer tabletStates, rand'')
      serverAsyncQueues = Mp.fromList $ U.map serverEIds $ \eId -> (eId, Sq.empty)
      tabletAsyncQueues = Mp.fromList $ U.map serverEIds $
        \eId ->
          let tabletAsyncQueuesForServer = Mp.fromList $ U.map (partitionConfig ^?! ix eId) $
                \tabletShape -> (tabletShape, Sq.empty)
          in (eId, tabletAsyncQueuesForServer)
      clocks = Mp.fromList $ U.map serverEIds  $ \eId -> (eId, 0)
  in Tt.TestState {
      Tt._rand = rand''',
      Tt._serverEIds = serverEIds,
      Tt._clientEIds = clientEIds,
      Tt._queues = queues,
      Tt._nonemptyQueues = nonemptyQueues,
      Tt._serverStates = serverStates,
      Tt._tabletStates = tabletStates,
      Tt._serverAsyncQueues = serverAsyncQueues,
      Tt._tabletAsyncQueues = tabletAsyncQueues,
      Tt._clocks = clocks,
      Tt._nextInt = 0,
      Tt._trueTimestamp = 0,
      Tt._clientMsgsReceived = St.empty
    }

addMsg :: Ms.Message -> (Co.EndpointId, Co.EndpointId) -> STB Tt.TestState ()
addMsg msg (fromEId, toEId) = do
  queue <- Tt.queues . ix fromEId . ix toEId .^^.* U.push msg
  if Sq.length queue == 1
    then Tt.nonemptyQueues .^^. St.insert (fromEId, toEId)
    else Tt.nonemptyQueues .^^. id
  return ()

pollMsg :: (Co.EndpointId, Co.EndpointId) -> STB Tt.TestState Ms.Message
pollMsg (fromEId, toEId) = do
  msg <- Tt.queues . ix fromEId . ix toEId .^^* U.poll
  queueLength <- Tt.queues . ix fromEId . ix toEId .^^^* Sq.length
  if queueLength == 0
    then Tt.nonemptyQueues .^^. St.delete (fromEId, toEId)
    else Tt.nonemptyQueues .^^. id
  return msg

runServerIAction :: Co.EndpointId -> Ac.S'InputAction -> STB Tt.TestState ()
runServerIAction eId iAction = do
  (_, msgsO) <- runT (Tt.serverStates . ix eId) (SIH.handleInputAction iAction)
  Mo.forM_ msgsO $ \msgO -> do
    case msgO of
      Ac.S'Send toEIds msg -> Mo.forM_ toEIds $ \toEId -> addMsg msg (eId, toEId)
      Ac.S'TabletForward tabletId clientEId tabletMsg -> do
        runTabletIAction eId tabletId (Ac.T'Receive clientEId tabletMsg)
      Ac.S'Print _ -> return ()

runTabletIAction
  :: Co.EndpointId
  -> Co.TabletShape
  -> Ac.T'InputAction
  -> STB Tt.TestState ()
runTabletIAction eId tabletId iAction = do
  (_, msgsO) <- runT (Tt.tabletStates . ix eId . ix tabletId) (TIH.handleInputAction iAction)
  Mo.forM_ msgsO $ \msgO -> do
    case msgO of
      Ac.T'Send toEIds msg -> Mo.forM_ toEIds $ \toEId -> addMsg msg (eId, toEId)
      Ac.T'Print _ -> return ()

deliverMessage :: (Co.EndpointId, Co.EndpointId) -> STB Tt.TestState ()
deliverMessage (fromEId, toEId) = do
  msg <- pollMsg (fromEId, toEId)
  serverEIds' <- getL $ Tt.serverEIds
  clientEIds' <- getL $ Tt.clientEIds
  let allEIds = serverEIds' ++ clientEIds'
      route
        | Li.elem toEId serverEIds' =
            runServerIAction toEId $ Ac.S'Receive fromEId msg
        | Li.elem toEId clientEIds' = do
            Tt.clientMsgsReceived .^^. St.insert msg
            return ()
        | otherwise = error $ "Can't deliver message; " ++ (show toEId) ++
                              " doesn't present in any existing " ++ " endpoint: " ++ (show allEIds)
  route

dropMessages :: Int -> STB Tt.TestState ()
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

-- Simulate one millisecond of execution. This involves incrementing the server's
-- clocks, handling any async tasks whose time has come to execute, and exchanging
-- `numMessages` number of messages.
simulate1ms :: STB Tt.TestState ()
simulate1ms = do
  -- increment each server's clocks and run async tasks that are now ready
  randPercent :: Int <- Tt.rand .^^ Rn.randomR (1, 100)
  if randPercent <= skewProb
    then do
      -- We have to put this behind a randPercentage with the the same
      -- probability of success as the Tt.clocks to prevent the trueTime from
      -- pulling away.
      Tt.trueTimestamp .^^. (+1)
      return ()
    else return ()
  serverEIds <- getL Tt.serverEIds
  Mo.forM_ serverEIds $ \eId -> do
    randPercent :: Int <- Tt.rand .^^ Rn.randomR (1, 100)
    if randPercent <= skewProb
      then do
        -- Increment clock
        clockVal <- Tt.clocks . ix eId .^^.* (+1)
        -- Process server async messages
        asyncQueue <- getT $ Tt.serverAsyncQueues . ix eId
        let (readyActions, remainder) = asyncQueue & Sq.spanl (\(_, time) -> time == clockVal)
        Tt.serverAsyncQueues . ix eId .^^.* \_ -> remainder
        Mo.forM_ readyActions $ \(iAction, _) -> runServerIAction eId iAction
        -- Process tablet async messages
        queuesRanges <- Tt.tabletAsyncQueues . ix eId .^^^* Mp.toList
        Mo.forM_ queuesRanges $ \(range, asyncQueue) -> do
          let (readyActions, remainder) = asyncQueue & Sq.spanl (\(_, time) -> time == clockVal)
          Tt.tabletAsyncQueues . ix eId . ix range .^^.* \_ -> remainder
          Mo.forM_ readyActions $ \(iAction, _) -> runTabletIAction eId range iAction
      else return ()
  -- Deliver messages
  --
  -- Here, we are delivering approximately as many messages as there are non-empty
  -- channels. We select which channels to deliver by first building a distribution,
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
simulateNms :: Int -> STB Tt.TestState ()
simulateNms n = do
  Mo.forM_ [1..n] $ \_ -> simulate1ms

-- Simulate execution until there are no more messages in any channel
-- or any asyncQueue.
simulateAll :: STB Tt.TestState ()
simulateAll = do
  let simulate =
        do
          length <- Tt.nonemptyQueues .^^^ St.size
          anySlaveTasks <- Tt.serverAsyncQueues .^^^ any (\q -> Sq.length q > 0)
          anyTabletTasks <- Tt.tabletAsyncQueues .^^^ any (\tablets -> tablets & any (\q -> Sq.length q > 0))
          if length > 0 || anySlaveTasks || anyTabletTasks
            then do
              simulate1ms
              simulate
            else return ()
  simulate

addAdminMsg
  :: Co.EndpointId
  -> Co.EndpointId
  -> Ms.Ad'Payload
  -> STB Tt.TestState Co.RequestId
addAdminMsg fromEId toEId payload = do
  int <- Tt.nextInt .^^. (+1)
  let requestId = (Co.RequestId (show int))
      msg = Ms.Admin $ Ms.Ad'Message (Ms.Ad'Metadata requestId) payload
  addMsg msg (fromEId, toEId)
  return requestId
