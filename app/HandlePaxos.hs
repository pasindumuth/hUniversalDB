{-# LANGUAGE RecordWildCards #-}
module HandlePaxos where

import qualified Data.Binary as B
import qualified Data.Binary.Put as BP
import qualified Data.Binary.Get as BG
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Default as D
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Control.Concurrent.Chan as C
import qualified Control.Concurrent.MVar as MV
import qualified Control.Monad.State as St
import qualified Network.Simple.TCP as TCP

import qualified Paxos as P

type EndpointId = String
type Connections = M.Map EndpointId (C.Chan P.PaxosMessage)

-- Primitive function for receiving a message
receiveMessage :: B.Binary a => TCP.Socket -> IO a
receiveMessage socket = do
  -- TODO handle socket closure properly
  Just headerB <- TCP.recv socket 8 -- receive fixed-size, 8 byte header
  let msgLen = B.decode (BSL.fromStrict headerB) :: Int -- B.decode takes 8 byte ByteStrings and decodes them as Int
  Just bodyB <- TCP.recv socket msgLen
  return (B.decode (BSL.fromStrict bodyB))

-- Primitive function for sending a message
sendMessage :: B.Binary a => TCP.Socket -> a -> IO ()
sendMessage socket msg = do
  let bodyB = BSL.toStrict (BP.runPut (B.put msg))
      headerB = BSL.toStrict (B.encode (BS.length bodyB))
  TCP.send socket headerB
  TCP.send socket bodyB

-- Thread function for receiving
handleSlaveReceive ::EndpointId -> C.Chan (EndpointId, P.PaxosMessage) -> TCP.Socket -> IO ()
handleSlaveReceive endpointId chan socket = do
  receiveLoop
  where
    receiveLoop = do
      message <- receiveMessage socket
      C.writeChan chan (endpointId, message)
      receiveLoop

-- Thread function for sending
handleSlaveSend :: C.Chan P.PaxosMessage -> TCP.Socket -> IO ()
handleSlaveSend chan socket = do
  sendLoop
  where
    sendLoop :: IO ()
    sendLoop = do
      msg <- C.readChan chan
      sendMessage socket msg
      sendLoop

-- Takes results of Paxos computation and sends the message
sendPaxosMessage :: (Maybe P.PaxosMessage) -> EndpointId -> MV.MVar Connections -> IO ()
sendPaxosMessage msgM endpointId connsM = do
  case msgM of
    Just msg -> do
      conns <- MV.readMVar connsM
      case M.lookup endpointId conns of
        Just chan -> do
          C.writeChan chan msg
        _ -> return ()
    _ -> return ()

-- Thread function of thread that manages everything paxos
handlePaxos :: C.Chan (EndpointId, P.PaxosMessage) -> MV.MVar Connections -> IO ()
handlePaxos chan conns = do
  let paxosIns = D.def :: P.PaxosInstance
  handlePaxosMessage paxosIns
  where
    handlePaxosMessage :: P.PaxosInstance -> IO ()
    handlePaxosMessage paxosIns@P.PaxosInstance{..} = do
      (endpointId, msg) <- C.readChan chan
      case msg of
          (P.Propose crnd cval) -> do
            let (msg, newS) = St.runState (P.propose crnd cval) proposerState
            sendPaxosMessage (Just msg) endpointId conns
            handlePaxosMessage paxosIns{ P.proposerState = newS }
          (P.Prepare crnd) -> do
            let (msgM, newS) = St.runState (P.prepare crnd) acceptorState
            sendPaxosMessage msgM endpointId conns
            handlePaxosMessage paxosIns{ P.acceptorState = newS }
          (P.Promise crnd vrnd vval) -> do
            let proposals = (P.proposals proposerState)
                Just proposal = M.lookup crnd proposals
                (msgM, newProposal) = St.runState (P.promise crnd vrnd vval) proposal
                newProposals = M.insert crnd newProposal proposals
                newS = proposerState { P.proposals = newProposals }
            sendPaxosMessage msgM endpointId conns
            handlePaxosMessage paxosIns{ P.proposerState = newS }
          (P.Accept crnd cval) -> do
            let (msgM, newS) = St.runState (P.accept crnd cval) acceptorState
            sendPaxosMessage msgM endpointId conns
            handlePaxosMessage paxosIns{ P.acceptorState = newS }
          (P.Learn lrnd lval) -> do
            let (_, newS) = St.runState (P.learn lrnd lval) learnerState
            handlePaxosMessage paxosIns{ P.learnerState = newS }


