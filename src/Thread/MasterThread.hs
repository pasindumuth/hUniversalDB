module Thread.MasterThread where

import qualified Control.Concurrent as Ct
import qualified Control.Concurrent.MVar as MV
import qualified Control.Monad as Mo
import qualified Data.Default as Df
import qualified Data.Map as Mp
import qualified Data.Maybe as Mb
import qualified System.Random as Rn

import qualified Infra.Utils as U
import qualified Net.Connections as Cn
import qualified Proto.Actions.MasterActions as MAc
import qualified Proto.Common as Co
import qualified Proto.Messages as Ms
import qualified Master.MasterInputHandler as MIH
import qualified Master.Env as En
import qualified Master.MasterState as MS
import Infra.Lens
import Infra.State

masterEIds = [
  Co.EndpointId "172.18.1.3",
  Co.EndpointId "172.18.1.4",
  Co.EndpointId "172.18.1.5",
  Co.EndpointId "172.18.1.6",
  Co.EndpointId "172.18.1.7"]

slaveEIds  = Mp.fromList [(
  Co.SlaveGroupId "slaveGroup1", [
    Co.EndpointId "172.18.0.3",
    Co.EndpointId "172.18.0.4",
    Co.EndpointId "172.18.0.5",
    Co.EndpointId "172.18.0.6",
    Co.EndpointId "172.18.0.7"])]

startMasterThread
  :: Rn.StdGen
  -> Ct.Chan (MAc.InputAction)
  -> MV.MVar (Cn.Connections Ms.Message)
  -> IO ()
startMasterThread rg iActionChan connM = do
  let (paxosId, rg') = U.mkUID rg & _1 %~ Co.PaxosId
      g = MS.constructor paxosId rg' masterEIds slaveEIds
  handleMessage g
  where
    handleMessage :: MS.MasterState -> IO ()
    handleMessage g = do
      iAction <- Ct.readChan iActionChan
      let (_, (oActions, _, g')) = runST (MIH.handleInputAction iAction) g
      conn <- MV.readMVar connM
      Mo.forM_ oActions $ \action ->
        case action of
          MAc.Send eIds msg -> do
            Mo.forM_ eIds $ \eId -> Mp.lookup eId conn & Mb.fromJust $ msg
          MAc.RetryOutput counterValue delay -> do
            Ct.forkIO $ do
              Ct.threadDelay (delay * 1000)
              Ct.writeChan iActionChan $ MAc.RetryInput counterValue
            return ()
          MAc.Print message -> do
            putStrLn message
          MAc.PerformOutput uid delay -> do
            Ct.forkIO $ do
              Ct.threadDelay (delay * 1000)
              Ct.writeChan iActionChan $ MAc.PerformInput uid
            return ()
      handleMessage g'
