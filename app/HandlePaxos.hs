{-# LANGUAGE RecordWildCards #-}
module HandlePaxos where

import qualified Data.Default as D
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Control.Concurrent as C
import qualified Control.Concurrent.MVar as MV
import qualified Control.Monad as Mo
import qualified Control.Monad.State as St

import qualified Paxos as P
import qualified Network as N

type Connections = M.Map N.EndpointId (C.Chan P.PaxosMessage)

addConn
  :: MV.MVar Connections
  -> N.EndpointId
  -> IO (C.Chan P.PaxosMessage)
addConn connM endpointId = do
   sendChan <- C.newChan
   conn <- MV.takeMVar connM
   MV.putMVar connM (M.insert endpointId sendChan conn)
   return sendChan

delConn
  :: MV.MVar Connections
  -> N.EndpointId -> IO ()
delConn connM endpointId = do
  conn <- MV.takeMVar connM
  MV.putMVar connM (M.delete endpointId conn)


-- Takes results of Paxos computation and sends the message
sendPaxosMessage :: (Maybe P.PaxosMessage) -> N.EndpointId -> MV.MVar Connections -> IO ()
sendPaxosMessage msgM endpointId connM = do
  case msgM of
    Just msg -> do
      conn <- MV.readMVar connM
      case M.lookup endpointId conn of
        Just chan -> do
          C.writeChan chan msg
        _ -> return ()
    _ -> return ()

broadcastPaxosMessage :: (Maybe P.PaxosMessage) -> MV.MVar Connections -> IO ()
broadcastPaxosMessage msgM connM = do
  case msgM of
    Just msg -> do
      conn <- MV.readMVar connM
      Mo.forM_ (M.toList conn) $ \(_, v) -> C.writeChan v msg
    _ -> return ()

-- Thread function of thread that manages everything paxos
handlePaxos:: C.Chan (N.EndpointId, P.PaxosMessage) -> MV.MVar Connections -> IO ()
handlePaxos chan connM = do
  let paxosIns = D.def :: P.PaxosInstance
  handlePaxosMessage paxosIns
  where
    handlePaxosMessage :: P.PaxosInstance -> IO ()
    handlePaxosMessage paxosIns@P.PaxosInstance{..} = do
      (endpointId, msg) <- C.readChan chan
      print "Handling Paxos"
      case msg of
          (P.Propose crnd cval) -> do
            print "Handling Propose"
            let (msg, newS) = St.runState (P.propose crnd cval) proposerState
            broadcastPaxosMessage (Just msg) connM
            handlePaxosMessage paxosIns{ P.proposerState = newS }
          (P.Prepare crnd) -> do
            print "Handling Prepare"
            let (msgM, newS) = St.runState (P.prepare crnd) acceptorState
            sendPaxosMessage msgM endpointId connM
            handlePaxosMessage paxosIns{ P.acceptorState = newS }
          (P.Promise crnd vrnd vval) -> do
            print "Handling Promise"
            let proposals = P.proposals proposerState
                Just proposal = M.lookup crnd proposals
                (msgM, newProposal) = St.runState (P.promise crnd vrnd vval) proposal
                newProposals = M.insert crnd newProposal proposals
                newS = proposerState { P.proposals = newProposals }
            broadcastPaxosMessage msgM connM
            handlePaxosMessage paxosIns{ P.proposerState = newS }
          (P.Accept crnd cval) -> do
            print "Handling Accept"
            let (msgM, newS) = St.runState (P.accept crnd cval) acceptorState
            broadcastPaxosMessage msgM connM
            handlePaxosMessage paxosIns{ P.acceptorState = newS }
          (P.Learn lrnd lval) -> do
            print "Handling Learn"
            let (learnedVal, newS) = St.runState (P.learn lrnd lval) learnerState
            case learnedVal of
              Just val -> print "Learned something!"
              _ -> return()
            handlePaxosMessage paxosIns{ P.learnerState = newS }
