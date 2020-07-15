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
import qualified Proto.Actions.SlaveActions as SAc
import qualified Proto.Common as Co
import qualified Proto.Messages as Ms
import qualified Thread.SlaveThread as ST
import Infra.Lens
import Infra.State

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

accept
  :: MV.MVar Cn.Connections
  -> Ct.Chan (Co.EndpointId, Ms.Message)
  -> IO ()
accept connM receiveChan =
  TCP.serve TCP.HostAny "8000" $ createConnHandlers connM receiveChan

connect
  :: String
  -> MV.MVar Cn.Connections
  -> Ct.Chan (Co.EndpointId, Ms.Message)
  -> IO ()
connect ip connM receiveChan =
  TCP.connect ip "8000" $ createConnHandlers connM receiveChan

handleReceive
  :: Ct.Chan (Co.EndpointId, Ms.Message)
  -> Ct.Chan (SAc.InputAction)
  -> IO ()
handleReceive receiveChan iActionChan = do
  Mo.forever $ do
    (eId, msg) <- Ct.readChan receiveChan
    Ct.writeChan iActionChan $ SAc.Receive eId msg

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
  Ct.forkIO $ accept connM receiveChan

  -- Initiate connections with other slaves
  Mo.forM_ otherIPs $ \ip -> Ct.forkIO $ connect ip connM receiveChan

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
