module Thread.TabletThread where

import qualified Control.Concurrent as Ct
import qualified Control.Concurrent.MVar as MV
import qualified Control.Monad as Mo
import qualified Data.Default as Df
import qualified Data.Map as Mp
import qualified Data.Maybe as Mb
import qualified System.Random as Rn

import qualified Net.Connections as Cn
import qualified Paxos.MultiPaxosInstance as MP
import qualified Proto.Actions.TabletActions as TAc
import qualified Proto.Common as Co
import qualified Slave.Tablet.TabletInputHandler as TIH
import qualified Slave.Tablet.Env as En
import qualified Slave.Tablet.TabletState as TS
import Infra.Lens
import Infra.State

slaveEIds = ["172.18.0.3", "172.18.0.4", "172.18.0.5", "172.18.0.6", "172.18.0.7"]

startTabletThread
  :: Rn.StdGen
  -> Co.TabletId
  -> Ct.Chan (TAc.InputAction)
  -> MV.MVar Cn.Connections
  -> IO ()
startTabletThread rg tabletId iActionChan connM = do
  let g = TS.constructor tabletId rg slaveEIds
  handleMessage g
  where
    handleMessage :: TS.TabletState -> IO ()
    handleMessage g = do
      iAction <- Ct.readChan iActionChan
      let (_, (oActions, _, g')) = runST (TIH.handleInputAction iAction) g
      conn <- MV.readMVar connM
      Mo.forM_ oActions $ \action ->
        case action of
          TAc.Send eIds msg -> Mo.forM_ eIds $
            \eId -> Mp.lookup eId conn & Mb.fromJust $ msg
          TAc.RetryOutput counterValue delay -> do
            Ct.forkIO $ do
              Ct.threadDelay (delay * 1000)
              Ct.writeChan iActionChan $ TAc.RetryInput counterValue
            return ()
          TAc.Print message -> do
            putStrLn message
      handleMessage g'
