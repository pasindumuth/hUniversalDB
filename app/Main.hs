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
import Control.Lens (makeLenses, (%~), (.~), (^.), (&))

import qualified Connections as CC
import qualified Logging as L
import qualified Network as N
import qualified MultiPaxos as MP
import qualified Message as M
import qualified MessageHandler as MH

handleSelfConn
  :: MV.MVar CC.Connections
  -> CC.EndpointId
  -> C.Chan (CC.EndpointId, M.Message)
  -> IO ()
handleSelfConn connM endpointId receiveChan = do
  sendChan <- CC.addConn connM endpointId
  L.infoM L.main "Created Self-Connections"
  Mo.forever $ do
    msg <- C.readChan sendChan
    C.writeChan receiveChan (endpointId, msg)

createConnHandlers
  :: MV.MVar CC.Connections
  -> C.Chan (CC.EndpointId, M.Message)
  -> (TCP.Socket, TCP.SockAddr)
  -> IO ()
createConnHandlers connM receiveChan (socket, remoteAddr) = do
  L.infoM L.main $ "Connection established to " ++ show remoteAddr
  let endpointId = show remoteAddr
  sendChan <- CC.addConn connM endpointId
  C.forkFinally (N.handleSend sendChan socket) $ \_ -> CC.delConn connM endpointId
  N.handleReceive endpointId receiveChan socket

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
  -> C.Chan (CC.EndpointId, M.MultiPaxosMessage)
  -> IO ()
handleReceive receiveChan paxosChan = do
  Mo.forever $ do
    (endpointId, msg) <- C.readChan receiveChan
    C.writeChan paxosChan (endpointId, MH.handleMessage msg)

handleMultiPaxosThread :: IO (CC.EndpointId, M.MultiPaxosMessage) -> MV.MVar CC.Connections -> IO ()
handleMultiPaxosThread getPaxosMsg connM = do
  handlePaxosMessage D.def
  where
    handlePaxosMessage :: MP.MultiPaxos -> IO ()
    handlePaxosMessage m = do
      (endpointId, multiPaxosMessage) <- getPaxosMsg
      conn <- MV.readMVar connM
      let endpointIds = Mp.toList conn & map fst 
          (msgsO, m') = MP.handleMultiPaxos m endpointIds endpointId multiPaxosMessage
      if (m ^. MP.paxosLog /= m' ^. MP.paxosLog)
        then L.infoM L.paxos $ show $ m' ^. MP.paxosLog
        else return ()
      Mo.forM_ msgsO $ \(endpointId, msgO) ->
        Mp.lookup endpointId conn & Mb.fromJust $ M.MMessage msgO 
      handlePaxosMessage m'

startSlave :: [String] -> IO ()
startSlave (curIP:otherIPs) = do
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
  handleMultiPaxosThread (C.readChan paxosChan) connM

startClient :: [String] -> IO ()
startClient (ip:message) = do
  L.infoM L.main "Starting client"
  TCP.connect ip "9000" $ \(socket, remoteAddr) -> do
    L.infoM L.main $ "Connection established to " ++ show remoteAddr
    Mo.forever $ do
      line <- getLine
      N.sendMessage socket $ M.ClientMessage line

main :: IO ()
main = do
  L.setupLogging
  args <- E.getArgs
  let (mode:rest) = args
  if mode == "server"
    then startSlave rest
    else startClient rest
