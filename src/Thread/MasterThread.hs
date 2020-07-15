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
import qualified Master.MasterInputHandler as MIH
import qualified Master.Env as En
import qualified Master.MasterState as MS
import Infra.Lens
import Infra.State

-- TODO: Actually change this
masterEIds = ["172.18.0.3", "172.18.0.4", "172.18.0.5", "172.18.0.6", "172.18.0.7"]

startMasterThread
  :: Rn.StdGen
  -> Ct.Chan (MAc.InputAction)
  -> MV.MVar Cn.Connections
  -> IO ()
startMasterThread rg iActionChan connM = do
  let g = MS.constructor "master" rg masterEIds
  handlePaxosMessage g
  where
    handlePaxosMessage :: MS.MasterState -> IO ()
    handlePaxosMessage g = do
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
      handlePaxosMessage g'
