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
import qualified InputActionHandler as IAH
import qualified Records.Actions.Actions as A
import qualified Records.Messages.ClientMessages as CM
import qualified Records.Messages.Messages as M
import qualified Records.Messages.PaxosMessages as PM
import qualified MessageHandler as MH
import qualified Utils as U
import Lens ((^.), (&), (.~), lp3, _1, _2)
import State
import qualified State as St

slaveEIds = ["172.18.0.3", "172.18.0.4", "172.18.0.5", "172.18.0.6", "172.18.0.7"]

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
  let eId = U.prefix  ':' $ show remoteAddr
  L.infoM L.main $ "Connection established to " ++ eId
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
  -> C.Chan (A.InputAction)
  -> IO ()
handleReceive receiveChan iActionChan = do
  Mo.forever $ do
    (eId, msg) <- C.readChan receiveChan
    C.writeChan iActionChan $ A.Receive eId msg

handleMultiPaxosThread
  :: R.StdGen
  -> C.Chan (A.InputAction)
  -> MV.MVar CC.Connections -> IO ()
handleMultiPaxosThread rg iActionChan connM = do
  let g = D.def & GS.env . EN.rand .~ rg & GS.env . EN.slaveEIds .~ slaveEIds
  handlePaxosMessage g
  where
    handlePaxosMessage :: GS.GlobalState -> IO ()
    handlePaxosMessage g = do
      iAction <- C.readChan iActionChan
      let (_, (oActions, g')) = runST (IAH.handleInputAction iAction) g
      conn <- MV.readMVar connM
      Mo.forM_ (reverse oActions) $ \action ->
        case action of
          A.Send eIds msg -> Mo.forM_ eIds $
            \eId -> Mp.lookup eId conn & Mb.fromJust $ msg
          A.RetryOutput counterValue -> do
            C.forkIO $ do
              C.threadDelay 100000
              C.writeChan iActionChan $ A.RetryInput counterValue
            return ()
          A.Print message -> do
            print message
      handlePaxosMessage g'

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

  -- Start accepting slave connections
  C.forkIO $ acceptClientConn connM receiveChan

  -- Setup message routing thread
  iActionChan <- C.newChan
  C.forkIO $ handleReceive receiveChan iActionChan

  -- Start Paxos handling thread
  let seed = read seedStr :: Int
      rg = R.mkStdGen seed
  handleMultiPaxosThread rg iActionChan connM

-- Example commands: r k 0, w k v 0
startClient :: [String] -> IO ()
startClient (ip:message) = do
  L.infoM L.main "Starting client"
  TCP.connect ip "9000" $ \(socket, remoteAddr) -> do
    L.infoM L.main $ "Connection established to " ++ show remoteAddr
    C.forkIO $ Mo.forever $ do
      msg <- N.receiveMessage socket
      print (msg :: M.Message)
    Mo.forever $ do
      line <- getLine
      case words line of
        ["r", key, timestamp] -> do
          N.sendMessage socket $ M.ClientRequest $ CM.ReadRequest key (read timestamp)
        ["w", key, value, timestamp] -> do
          N.sendMessage socket $ M.ClientRequest $ CM.WriteRequest key value (read timestamp)
        _ -> print "Unrecognized command or number of arguments"

main :: IO ()
main = do
  L.setupLogging
  args <- E.getArgs
  let (mode:rest) = args
  if mode == "server"
    then startSlave rest
    else startClient rest
