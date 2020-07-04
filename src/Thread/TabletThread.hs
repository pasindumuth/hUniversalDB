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
import qualified Proto.Actions.Actions as Ac
import qualified Proto.Common as Co
import qualified Slave.Tablet.TabletInputHandler as TIH
import qualified Slave.Tablet.Env as En
import qualified Slave.Tablet.TabletState as TS
import Infra.Lens
import Infra.State

slaveEIds = ["172.18.0.3", "172.18.0.4", "172.18.0.5", "172.18.0.6", "172.18.0.7"]

startTabletThread
  :: Rn.StdGen
  -> Co.KeySpaceRange
  -> Ct.Chan (Ac.TabletInputAction)
  -> MV.MVar Cn.Connections
  -> IO ()
startTabletThread rg keySpaceRange iActionChan connM = do
  let g = Df.def & TS.env . En.rand .~ rg
                 & TS.env . En.slaveEIds .~ slaveEIds
                 & TS.range .~ keySpaceRange
                 & TS.multiPaxosInstance . MP.paxosId .~ (show keySpaceRange)
  handlePaxosMessage g
  where
    handlePaxosMessage :: TS.TabletState -> IO ()
    handlePaxosMessage g = do
      iAction <- Ct.readChan iActionChan
      let (_, (oActions, _, g')) = runST (TIH.handleInputAction iAction) g
      conn <- MV.readMVar connM
      Mo.forM_ (reverse oActions) $ \action ->
        case action of
          Ac.Send eIds msg -> Mo.forM_ eIds $
            \eId -> Mp.lookup eId conn & Mb.fromJust $ msg
          Ac.RetryOutput counterValue -> do
            Ct.forkIO $ do
              Ct.threadDelay 100000
              Ct.writeChan iActionChan $ Ac.TabletRetryInput counterValue
            return ()
          Ac.Print message -> do
            print message
      handlePaxosMessage g'
