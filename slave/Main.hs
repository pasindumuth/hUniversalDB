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

import qualified Connections as Cn
import qualified InputActionHandler as IAH
import qualified Infra.Logging as Lg
import qualified Infra.Utils as U
import qualified Proto.Actions.Actions as Ac
import qualified Proto.Common as Co
import qualified Proto.Messages as Ms
import qualified Records.Env as En
import qualified Records.GlobalState as GS
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

handleMultiPaxosThread
  :: Rn.StdGen
  -> Ct.Chan (Ac.InputAction)
  -> MV.MVar Cn.Connections -> IO ()
handleMultiPaxosThread rg iActionChan connM = do
  let g = Df.def & GS.env . En.rand .~ rg & GS.env . En.slaveEIds .~ slaveEIds
  handlePaxosMessage g
  where
    handlePaxosMessage :: GS.GlobalState -> IO ()
    handlePaxosMessage g = do
      iAction <- Ct.readChan iActionChan
      let (_, (oActions, g')) = runST (IAH.handleInputAction iAction) g
      conn <- MV.readMVar connM
      Mo.forM_ (reverse oActions) $ \action ->
        case action of
          Ac.Send eIds msg -> Mo.forM_ eIds $
            \eId -> Mp.lookup eId conn & Mb.fromJust $ msg
          Ac.RetryOutput counterValue -> do
            Ct.forkIO $ do
              Ct.threadDelay 100000
              Ct.writeChan iActionChan $ Ac.RetryInput counterValue
            return ()
          Ac.Print message -> do
            print message
      handlePaxosMessage g'

startSlave :: [String] -> IO ()
startSlave (seedStr:curIP:otherIPs) = do
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
  handleMultiPaxosThread rg iActionChan connM

main :: IO ()
main = do
  Lg.setupLogging
  args <- SE.getArgs
  startSlave args
