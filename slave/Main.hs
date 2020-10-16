module Main where

import qualified Control.Concurrent as Ct
import qualified Control.Concurrent.MVar as MV
import qualified Control.Monad as Mo
import qualified Data.Map as Mp
import qualified System.Environment as SE
import qualified System.Random as Rn

import qualified Infra.Logging as Lg
import qualified Net.Connections as Cn
import qualified Proto.Actions.SlaveActions as SAc
import qualified Proto.Common as Co
import qualified Proto.Messages as Ms
import qualified Thread.SlaveThread as ST
import Infra.Lens
import Infra.State

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
  Ct.forkIO $ Cn.selfConnect curIP connM receiveChan
  -- Start accepting slave connections
  Ct.forkIO $ Cn.accept connM receiveChan
  -- Initiate connections with other slaves
  Mo.forM_ otherIPs $ \ip -> Ct.forkIO $ Cn.connect ip connM receiveChan
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
