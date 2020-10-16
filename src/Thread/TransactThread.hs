module Thread.TransactThread where

import qualified Control.Concurrent as Ct
import qualified Control.Concurrent.MVar as MV
import qualified Control.Monad as Mo
import qualified Data.Default as Df
import qualified Data.Map as Mp
import qualified Data.Maybe as Mb
import qualified System.Random as Rn

import qualified Infra.Utils as U
import qualified Net.Connections as Cn
import qualified Proto.Actions.TransactActions as TrAc
import qualified Proto.Actions.TransactTabletActions as TTAc
import qualified Proto.Common as Co
import qualified Transact.TransactInputHandler as TIH
import qualified Transact.TransactState as TS
import Infra.Lens
import Infra.State

transactEIds = [
  Co.EndpointId "172.18.1.3",
  Co.EndpointId "172.18.1.4",
  Co.EndpointId "172.18.1.5",
  Co.EndpointId "172.18.1.6",
  Co.EndpointId "172.18.1.7"]

-- The keys are the chans used to forward messages to the Transact Tablets. 
type TabletMap = Mp.Map Co.PartitionShape (Ct.Chan TTAc.InputAction)

startTransactThread
  :: Rn.StdGen
  -> TabletMap
  -> Ct.Chan (TrAc.InputAction)
  -> MV.MVar Cn.Connections
  -> IO ()
startTransactThread rg tabletMap iActionChan connM = do
  let g = TS.constructor rg
  handleMessage g
  where
    handleMessage :: TS.TransactState -> IO ()
    handleMessage g = do
      iAction <- Ct.readChan iActionChan
      let (_, (oActions, _, g')) = runST (TIH.handleInputAction iAction) g
      conn <- MV.readMVar connM
      Mo.forM_ oActions $ \action ->
        case action of
          TrAc.Send eIds msg -> do
            Mo.forM_ eIds $ \eId -> Mp.lookup eId conn & Mb.fromJust $ msg
          TrAc.RetryOutput counterValue delay -> do
            Ct.forkIO $ do
              Ct.threadDelay (delay * 1000)
              Ct.writeChan iActionChan $ TrAc.RetryInput counterValue
            return ()
          TrAc.Print message -> do
            putStrLn message
            return ()
          TrAc.TabletForward tabletId eId msg -> do
            let tabletIActionChan = tabletMap ^?! ix tabletId
            Ct.writeChan tabletIActionChan $ TTAc.Receive eId msg
            return ()
      handleMessage g'
