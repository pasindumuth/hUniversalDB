{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Control.Concurrent as C
import qualified Control.Concurrent.MVar as MV
import qualified Control.Monad as Mo
import qualified Data.Map as Mp
import qualified Network.Simple.TCP as TCP
import qualified System.Environment as E
import qualified System.Log.Logger as L

import qualified Network as N
import qualified MultiPaxos as MP
import qualified Message as M

logM :: String -> IO ()
logM msg = L.logM "main" L.INFO msg

handleSelfConn
  :: MV.MVar N.Connections
  -> N.EndpointId
  -> C.Chan (N.EndpointId, M.Message)
  -> IO ()
handleSelfConn connM endpointId receiveChan = do
  sendChan <- N.addConn connM endpointId
  logM "Created Self-Connections"
  Mo.forever $ do
    msg <- C.readChan sendChan
    C.writeChan receiveChan (endpointId, msg)

createConnHandlers
  :: MV.MVar N.Connections
  -> C.Chan (N.EndpointId, M.Message)
  -> (TCP.Socket, TCP.SockAddr)
  -> IO ()
createConnHandlers connM receiveChan (socket, remoteAddr) = do
  logM $ "Connection established to " ++ show remoteAddr
  let endpointId = show remoteAddr
  sendChan <- N.addConn connM endpointId
  C.forkFinally (N.handleSend sendChan socket) $ \_ -> N.delConn connM endpointId
  N.handleReceive endpointId receiveChan socket

acceptSlaveConn
  :: MV.MVar N.Connections
  -> C.Chan (N.EndpointId, M.Message)
  -> IO ()
acceptSlaveConn connM receiveChan =
  TCP.serve TCP.HostAny "8000" $ createConnHandlers connM receiveChan

connectToSlave
  :: String
  -> MV.MVar N.Connections
  -> C.Chan (N.EndpointId, M.Message)
  -> IO ()
connectToSlave ip connM receiveChan =
  TCP.connect ip "8000" $ createConnHandlers connM receiveChan

acceptClientConn
  :: MV.MVar N.Connections
  -> C.Chan (N.EndpointId, M.Message)
  -> IO ()
acceptClientConn connM receiveChan =
  TCP.serve TCP.HostAny "9000" $ createConnHandlers connM receiveChan

handleReceive
  :: C.Chan (N.EndpointId, M.Message)
  -> C.Chan (N.EndpointId, M.MultiPaxosMessage)
  -> IO ()
handleReceive receiveChan paxosChan = do
  Mo.forever $ do
    (endpointId, msg) <- C.readChan receiveChan
    case msg of
      M.MMessage mpMsg -> C.writeChan paxosChan (endpointId, mpMsg)
      M.ClientMessage val -> C.writeChan paxosChan (endpointId, M.Insert $ M.Write val)

startSlave :: [String] -> IO ()
startSlave (curIP:otherIPs) = do
  logM "Start slave"

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
  MP.handleMultiPaxos paxosChan connM


startClient :: [String] -> IO ()
startClient (ip:message) = do
  logM "Starting client"
  TCP.connect ip "9000" $ \(socket, remoteAddr) -> do
    logM $ "Connection established to " ++ show remoteAddr
    Mo.forever $ do
      line <- getLine
      N.sendMessage socket $ M.Propose 10 $ M.Write line

main :: IO ()
main = do
  L.updateGlobalLogger L.rootLoggerName $ L.setLevel L.DEBUG
  args <- E.getArgs
  let (mode:rest) = args
  if mode == "server"
    then startSlave rest
    else startClient rest
