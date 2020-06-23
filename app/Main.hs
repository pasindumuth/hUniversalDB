{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Control.Concurrent as C
import qualified Control.Concurrent.MVar as MV
import qualified Control.Monad as Mo
import qualified Data.Default as D
import qualified Data.Map as Mp
import qualified Data.Maybe as Mb
import qualified Network.Simple.TCP as TCP
import qualified System.Environment as E
import qualified System.Random as R

import qualified Connections as CC
import qualified Logging as L
import qualified Network as N
import qualified Records.Env as EN
import qualified Records.GlobalState as GS
import qualified MultiPaxosInstance as MP
import qualified Records.Actions.Actions as A
import qualified Records.Messages.ClientMessages as CM
import qualified Records.Messages.Messages as M
import qualified Records.Messages.PaxosMessages as PM
import qualified MessageHandler as MH
import qualified Utils as U
import Lens ((^.), (&), (.~), lp3, (.^.), _1, _2)
import State (runST)
import qualified State as St

handleSelfConn
  :: MV.MVar CC.Connections
  -> CC.EndpointId
  -> C.Chan (CC.EndpointId, M.Message)
  -> IO ()
handleSelfConn connM eId receiveChan = do
  sendChan <- CC.addConn connM eId
  L.infoM L.main "Created Self-Connections"
  Mo.forever $ do
    msg <- C.readChan sendChan
    C.writeChan receiveChan (eId, msg)

createConnHandlers
  :: MV.MVar CC.Connections
  -> C.Chan (CC.EndpointId, M.Message)
  -> (TCP.Socket, TCP.SockAddr)
  -> IO ()
createConnHandlers connM receiveChan (socket, remoteAddr) = do
  L.infoM L.main $ "Connection established to " ++ show remoteAddr
  let eId = show remoteAddr
  sendChan <- CC.addConn connM eId
  C.forkFinally (N.handleSend sendChan socket) $ \_ -> CC.delConn connM eId
  N.handleReceive eId receiveChan socket

acceptSlaveConn
  :: MV.MVar CC.Connections
  -> C.Chan (CC.EndpointId, M.Message)
  -> IO ()
acceptSlaveConn connM receiveChan =
  TCP.serve TCP.HostAny "8000" $ createConnHandlers connM receiveChan

connectToSlave
  :: String
  -> MV.MVar CC.Connections
  -> C.Chan (CC.EndpointId, M.Message)
  -> IO ()
connectToSlave ip connM receiveChan =
  TCP.connect ip "8000" $ createConnHandlers connM receiveChan

acceptClientConn
  :: MV.MVar CC.Connections
  -> C.Chan (CC.EndpointId, M.Message)
  -> IO ()
acceptClientConn connM receiveChan =
  TCP.serve TCP.HostAny "9000" $ createConnHandlers connM receiveChan

handleReceive
  :: C.Chan (CC.EndpointId, M.Message)
  -> C.Chan (CC.EndpointId, PM.MultiPaxosMessage)
  -> IO ()
handleReceive receiveChan paxosChan = do
  Mo.forever $ do
    (eId, msg) <- C.readChan receiveChan
    C.writeChan paxosChan (eId, MH.handleMessage msg)

-- TODO: client connections should be passing the messages into
-- a the IMHIO
handleMultiPaxosThread
  :: R.StdGen
  -> IO (CC.EndpointId, PM.MultiPaxosMessage)
  -> MV.MVar CC.Connections -> IO ()
handleMultiPaxosThread rg getPaxosMsg connM = do
  let g = D.def & GS.env . EN.rand .~ rg
  handlePaxosMessage g
  where
    handlePaxosMessage :: GS.GlobalState -> IO ()
    handlePaxosMessage g = do
      (eId, multiPaxosMessage) <- getPaxosMsg
      conn <- MV.readMVar connM
      let eIds = Mp.toList conn & map fst
          g' = g & GS.env . EN.slaveEIds .~ eIds
          (_, (msgsO, g'')) = runST ((lp3 (GS.multiPaxosInstance, GS.paxosLog, GS.env))
            St..^ MP.handleMultiPaxos eId multiPaxosMessage) g'
      if (g' ^. GS.paxosLog /= g'' ^. GS.paxosLog)
        then L.infoM L.paxos $ show $ g'' ^. GS.paxosLog
        else return ()
      Mo.forM_ msgsO $ \msgO ->
        case msgO of
          A.Send eIds msg -> Mo.forM_ eIds $
            \eId -> Mp.lookup eId conn & Mb.fromJust $ msg
      handlePaxosMessage g''

startSlave :: [String] -> IO ()
startSlave (seedStr:curIP:otherIPs) = do
  L.infoM L.main "Start slave"

  -- Create PaxosChan
  receiveChan <- C.newChan

  -- Create the Slave Connections
  connM <- MV.newMVar Mp.empty

  -- Create a single thread to handle the self connection
  C.forkIO $ handleSelfConn connM curIP receiveChan

  -- Start accepting slave connections
  C.forkIO $ acceptSlaveConn connM receiveChan

  -- Initiate connections with other slaves
  Mo.forM_ otherIPs $ \ip -> C.forkIO $ connectToSlave ip connM receiveChan

  -- Create the Client Connections object
  clientConnM <- MV.newMVar Mp.empty

  -- Start accepting slave connections
  C.forkIO $ acceptClientConn clientConnM receiveChan

  -- Setup message routing thread
  paxosChan <- C.newChan
  C.forkIO $ handleReceive receiveChan paxosChan

  -- Start Paxos handling thread
  let seed = read seedStr :: Int
      rg = R.mkStdGen seed
  handleMultiPaxosThread rg (C.readChan paxosChan) connM

startClient :: [String] -> IO ()
startClient (ip:message) = do
  L.infoM L.main "Starting client"
  TCP.connect ip "9000" $ \(socket, remoteAddr) -> do
    L.infoM L.main $ "Connection established to " ++ show remoteAddr
    Mo.forever $ do
      line <- getLine
      N.sendMessage socket $ M.ClientRequest $ CM.ReadRequest line 0

main :: IO ()
main = do
  L.setupLogging
  args <- E.getArgs
  let (mode:rest) = args
  if mode == "server"
    then startSlave rest
    else startClient rest
