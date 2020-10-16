module Main where

import qualified Control.Concurrent as Ct
import qualified Control.Concurrent.MVar as MV
import qualified Control.Monad as Mo
import qualified Data.Map as Mp
import qualified System.Environment as SE
import qualified System.Random as Rn

import qualified Infra.Logging as Lg
import qualified Infra.Utils as U
import qualified Net.Connections as Cn
import qualified Proto.Actions.TransactActions as TrAc
import qualified Proto.Common as Co
import qualified Proto.Messages as Ms
import qualified Thread.TransactThread as TrT
import qualified Thread.TransactTabletThread as TTT
import Infra.Lens
import Infra.State

partitionConfig :: Mp.Map Co.EndpointId [Co.PartitionShape]
partitionConfig = Mp.fromList [
  (Co.EndpointId "172.18.1.3", [
    Co.PartitionShape (Co.Path "table1") (Co.KeyRange Nothing Nothing)]),
  (Co.EndpointId "172.18.1.4", [
    Co.PartitionShape (Co.Path "table2") (Co.KeyRange Nothing (Just "j"))]),
  (Co.EndpointId "172.18.1.5", [
    Co.PartitionShape (Co.Path "table2") (Co.KeyRange (Just "j") Nothing),
    Co.PartitionShape (Co.Path "table3") (Co.KeyRange Nothing (Just "d")),
    Co.PartitionShape (Co.Path "table4") (Co.KeyRange Nothing (Just "k"))]),
  (Co.EndpointId "172.18.1.6", [
    Co.PartitionShape (Co.Path "table3") (Co.KeyRange (Just "d") (Just "p"))]),
  (Co.EndpointId "172.18.1.7", [
    Co.PartitionShape (Co.Path "table3") (Co.KeyRange (Just "p") Nothing),
    Co.PartitionShape (Co.Path "table4") (Co.KeyRange (Just "k") Nothing)])]

handleReceive
  :: Ct.Chan (Co.EndpointId, Ms.Message)
  -> Ct.Chan (TrAc.InputAction)
  -> IO ()
handleReceive receiveChan iActionChan = do
  Mo.forever $ do
    (eId, msg) <- Ct.readChan receiveChan
    Ct.writeChan iActionChan $ TrAc.Receive eId msg

startTransact :: String -> String -> [String] -> IO ()
startTransact seedStr curIP otherIPs = do
  Lg.infoM Lg.main "Start transact"
  -- Create PaxosChan
  receiveChan <- Ct.newChan
  -- Create the Transact Connections
  connM <- MV.newMVar Mp.empty
  -- Create a single thread to handle the self connection
  Ct.forkIO $ Cn.selfConnect curIP connM receiveChan
  -- Start accepting transact connections
  Ct.forkIO $ Cn.accept connM receiveChan
  -- Initiate connections with other transacts
  Mo.forM_ otherIPs $ \ip -> Ct.forkIO $ Cn.connect ip connM receiveChan
  -- Setup message routing thread
  iActionChan <- Ct.newChan
  Ct.forkIO $ handleReceive receiveChan iActionChan

  -- Initialize all tablet threads based on the Partitions that this Transact node should
  -- be managing. This also returns tabletMap, which is how the main Transact Thread forwards
  -- messages to the Transact Tablet Threads.
  (rg, tabletMap) <- U.foldM (Rn.mkStdGen $ read seedStr, Mp.empty) (partitionConfig ^. ix (Co.EndpointId curIP)) $
    \(rg, tabletMap) partitionShape -> do
      let (seed, rg') = Rn.random rg
      chan <- Ct.newChan
      let tabletMap' = Mp.insert partitionShape chan tabletMap
      -- Create the Transact Tablet Thread
      Ct.forkIO $ TTT.startTransactTabletThread (Rn.mkStdGen seed) partitionShape chan connM
      return (rg', tabletMap')

  -- Start Paxos handling thread
  TrT.startTransactThread rg tabletMap iActionChan connM

main :: IO ()
main = do
  Lg.setupLogging
  args <- SE.getArgs
  let (seedStr:curIP:otherIPs) = args
  startTransact seedStr curIP otherIPs
