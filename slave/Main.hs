{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Control.Concurrent as Ct
import qualified Control.Concurrent.MVar as MV
import qualified Control.Monad as Mo
import qualified Data.Default as Df
import qualified Data.Map as Mp
import qualified Data.Maybe as Mb
import qualified Network.Simple.TCP as TCP
import qualified System.Environment as SE
import qualified System.Random as Rn

import qualified Infra.Logging as Lg
import qualified Infra.Utils as U
import qualified Net.Connections as Cn
import qualified Proto.Actions.Actions as Ac
import qualified Proto.Common as Co
import qualified Proto.Messages as Ms
import qualified Slave.Tablet.TabletInputHandler as TIH
import qualified Slave.Tablet.Env as En
import qualified Slave.Tablet.TabletState as TS
import qualified Thread.SlaveThread as ST
import Infra.Lens
import Infra.State

slaveEIds = ["172.18.0.3", "172.18.0.4", "172.18.0.5", "172.18.0.6", "172.18.0.7"]

handleSelfConn
  :: MV.MVar Cn.Connections
  -> Co.EndpointId
  -> Ct.Chan (Co.EndpointId, Ms.Message)
  -> IO ()
handleSelfConn connM eId receiveChan = do
  sendChan <- Cn.addConn connM eId
  Lg.infoM Lg.main "Created Self-Connections"
  Mo.forever $ do
    msg <- Ct.readChan sendChan
    Ct.writeChan receiveChan (eId, msg)

createConnHandlers
  :: MV.MVar Cn.Connections
  -> Ct.Chan (Co.EndpointId, Ms.Message)
  -> (TCP.Socket, TCP.SockAddr)
  -> IO ()
createConnHandlers connM receiveChan (socket, remoteAddr) = do
  let eId = U.prefix  ':' $ show remoteAddr
  Lg.infoM Lg.main $ "Connection established to " ++ eId
  sendChan <- Cn.addConn connM eId
  Ct.forkFinally (Cn.handleSend sendChan socket) $ \_ -> Cn.delConn connM eId
  Cn.handleReceive eId receiveChan socket

acceptSlaveConn
  :: MV.MVar Cn.Connections
  -> Ct.Chan (Co.EndpointId, Ms.Message)
  -> IO ()
acceptSlaveConn connM receiveChan =
  TCP.serve TCP.HostAny "8000" $ createConnHandlers connM receiveChan

connectToSlave
  :: String
  -> MV.MVar Cn.Connections
  -> Ct.Chan (Co.EndpointId, Ms.Message)
  -> IO ()
connectToSlave ip connM receiveChan =
  TCP.connect ip "8000" $ createConnHandlers connM receiveChan

acceptClientConn
  :: MV.MVar Cn.Connections
  -> Ct.Chan (Co.EndpointId, Ms.Message)
  -> IO ()
acceptClientConn connM receiveChan =
  TCP.serve TCP.HostAny "9000" $ createConnHandlers connM receiveChan

handleReceive
  :: Ct.Chan (Co.EndpointId, Ms.Message)
  -> Ct.Chan (Ac.InputAction)
  -> IO ()
handleReceive receiveChan iActionChan = do
  Mo.forever $ do
    (eId, msg) <- Ct.readChan receiveChan
    Ct.writeChan iActionChan $ Ac.Receive eId msg

startSlave :: String -> String -> [String] -> IO ()
startSlave seedStr curIP otherIPs = do
  Lg.infoM Lg.main "Start slave"

  -- Create PaxosChan
  receiveChan <- Ct.newChan

  -- Create the Slave Connections
  connM <- MV.newMVar Mp.empty

  -- Create a single thread to handle the self connection
  Ct.forkIO $ handleSelfConn connM curIP receiveChan

  -- Start accepting slave connections
  Ct.forkIO $ acceptSlaveConn connM receiveChan

  -- Initiate connections with other slaves
  Mo.forM_ otherIPs $ \ip -> Ct.forkIO $ connectToSlave ip connM receiveChan

  -- Start accepting slave connections
  Ct.forkIO $ acceptClientConn connM receiveChan

  -- Setup message routing thread
  iActionChan <- Ct.newChan
  Ct.forkIO $ handleReceive receiveChan iActionChan

  -- Start Paxos handling thread
  let seed = read seedStr :: Int
      rg = Rn.mkStdGen seed
  ST.startSlaveThread rg iActionChan connM

main :: IO ()
main = do
  Lg.setupLogging
  args <- SE.getArgs
  let (seedStr:curIP:otherIPs) = args
  startSlave seedStr curIP otherIPs
