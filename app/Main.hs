{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import qualified Control.Concurrent as C
import qualified Control.Concurrent.MVar as MV
import qualified Control.Monad as Mo
import qualified Data.Map as M
import qualified Network.Simple.TCP as TCP
import qualified System.Environment as E

import qualified Network as N
import qualified Paxos as P
import qualified HandlePaxos as HP

handleSelfConn
  :: MV.MVar HP.Connections
  -> N.EndpointId
  -> C.Chan (N.EndpointId, P.PaxosMessage)
  -> IO ()
handleSelfConn connM endpointId paxosChan = do
  sendChan <- HP.addConn connM endpointId
  print $ "Created Self-Connections"
  Mo.forever $ do
    msg <- C.readChan sendChan
    C.writeChan paxosChan (endpointId, msg)

createConnHandlers
  :: MV.MVar HP.Connections
  -> C.Chan (N.EndpointId, P.PaxosMessage)
  -> (TCP.Socket, TCP.SockAddr)
  -> IO ()
createConnHandlers connM paxosChan (socket, remoteAddr) = do
  print $ "Connection established to " ++ show remoteAddr
  let endpointId = show remoteAddr
  sendChan <- HP.addConn connM endpointId
  C.forkFinally (N.handleSend sendChan socket) $ \_ -> HP.delConn connM endpointId
  N.handleReceive endpointId paxosChan socket

acceptSlaveConn
  :: MV.MVar HP.Connections
  -> C.Chan (N.EndpointId, P.PaxosMessage)
  -> IO ()
acceptSlaveConn connM paxosChan =
  TCP.serve TCP.HostAny "8000" $ createConnHandlers connM paxosChan

connectToSlave
  :: String
  -> MV.MVar HP.Connections
  -> C.Chan (N.EndpointId, P.PaxosMessage)
  -> IO ()
connectToSlave ip connM paxosChan =
  TCP.connect ip "8000" $ createConnHandlers connM paxosChan

acceptClientConn
  :: MV.MVar HP.Connections
  -> C.Chan (N.EndpointId, P.PaxosMessage)
  -> IO ()
acceptClientConn connM paxosChan =
  TCP.serve TCP.HostAny "9000" $ createConnHandlers connM paxosChan

startSlave :: [String] -> IO ()
startSlave (curIP:otherIPs) = do
  print "Start slave"

  -- Create PaxosChan
  paxosChan <- C.newChan

  -- Create the Slave Connections
  connM <- MV.newMVar M.empty

  -- Create a single thread to handle the self connection
  C.forkIO $ handleSelfConn connM curIP paxosChan

  -- Start accepting slave connections
  C.forkIO $ acceptSlaveConn connM paxosChan

  -- Initiate connections with other slaves
  Mo.forM_ otherIPs $ \ip -> C.forkIO $ connectToSlave ip connM paxosChan

  -- Create the Client Connections object
  clientConnM <- MV.newMVar M.empty

  -- Start accepting slave connections
  C.forkIO $ acceptClientConn clientConnM paxosChan

  -- Start Paxos handling thread
  HP.handlePaxos paxosChan connM


startClient :: [String] -> IO ()
startClient (ip:message) = do
  print "Starting client"
  TCP.connect ip "9000" $ \(socket, remoteAddr) -> do
    print $ "Connection established to " ++ show remoteAddr
    Mo.forever $ do
      line <- getLine
      N.sendMessage socket (P.Propose 10 (N.encode line))

main :: IO ()
main = do
  args <- E.getArgs
  let (mode:rest) = args
  if mode == "server"
    then startSlave rest
    else startClient rest
