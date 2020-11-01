module Main where

import qualified Control.Concurrent as Ct
import qualified Control.Concurrent.MVar as MV
import qualified Control.Monad as Mo
import qualified Data.Map as Mp
import qualified System.Environment as SE
import qualified System.Random as Rn

import qualified Common.Model.RelationalTablet as RT
import qualified Infra.Logging as Lg
import qualified Infra.Utils as U
import qualified Net.Connections as Cn
import qualified Transact.Model.Actions as Ac
import qualified Transact.Model.Common as Co
import qualified Transact.Model.Message as Ms
import qualified Transact.Thread.ServerThread as ST
import qualified Transact.Thread.TabletThread as TT
import Infra.Lens

-- | Hard simple coded table schema we will use for all tables.
schema :: RT.Schema
schema = RT.Schema [("key", RT.CT'String, True), ("value", RT.CT'Int, False)]

-- | A simpe utility for making Co.TabletKeyRanges for the specific schema above
mkKeyRange :: Maybe String -> Maybe String -> Co.TabletKeyRange
mkKeyRange startKeyM endKeyM =
  Co.TabletKeyRange (fmap toPrimary startKeyM) (fmap toPrimary endKeyM)
  where
    toPrimary key = RT.PrimaryKey [RT.CV'String key]

-- | Hard coded table partitioning configurations. We only handle 5 servers and
-- only a handful of tables. We don't support SQL DDL yet.
partitionConfig :: Mp.Map Co.EndpointId [Co.TabletShape]
partitionConfig = Mp.fromList [
  (Co.EndpointId "172.18.0.3", [
    Co.TabletShape (Co.TabletPath "table1") (mkKeyRange Nothing Nothing)]),
  (Co.EndpointId "172.18.0.4", [
    Co.TabletShape (Co.TabletPath "table2") (mkKeyRange Nothing (Just "j"))]),
  (Co.EndpointId "172.18.0.5", [
    Co.TabletShape (Co.TabletPath "table2") (mkKeyRange (Just "j") Nothing),
    Co.TabletShape (Co.TabletPath "table3") (mkKeyRange Nothing (Just "d")),
    Co.TabletShape (Co.TabletPath "table4") (mkKeyRange Nothing (Just "k"))]),
  (Co.EndpointId "172.18.0.6", [
    Co.TabletShape (Co.TabletPath "table3") (mkKeyRange (Just "d") (Just "p"))]),
  (Co.EndpointId "172.18.0.7", [
    Co.TabletShape (Co.TabletPath "table3") (mkKeyRange (Just "p") Nothing),
    Co.TabletShape (Co.TabletPath "table4") (mkKeyRange (Just "k") Nothing)])]

handleReceive
  :: Ct.Chan (Co.EndpointId, Ms.Message)
  -> Ct.Chan (Ac.S'InputAction)
  -> IO ()
handleReceive receiveChan iActionChan = do
  Mo.forever $ do
    (eId, msg) <- Ct.readChan receiveChan
    Ct.writeChan iActionChan $ Ac.S'Receive eId msg

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
  let tabletShapes = partitionConfig ^?! ix (Co.EndpointId curIP)
  (rg, tabletMap) <- U.foldM (Rn.mkStdGen $ read seedStr, Mp.empty) tabletShapes $
    \(rg, tabletMap) partitionShape -> do
      let (seed, rg') = Rn.random rg
      chan <- Ct.newChan
      let tabletMap' = Mp.insert partitionShape chan tabletMap
      -- Create the Transact Tablet Thread
      Ct.forkIO $ TT.startTabletThread (Rn.mkStdGen seed) partitionShape chan connM
      return (rg', tabletMap')

  -- Start Paxos handling thread
  let shapesWithSchema = U.map tabletShapes $ \s -> (schema, s)
  ST.startServerThread shapesWithSchema rg tabletMap iActionChan connM

main :: IO ()
main = do
  Lg.setupLogging
  args <- SE.getArgs
  let (seedStr:curIP:otherIPs) = args
  startTransact seedStr curIP otherIPs
