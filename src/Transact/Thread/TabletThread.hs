module Transact.Thread.TabletThread where

import qualified Control.Concurrent as Ct
import qualified Control.Concurrent.MVar as MV
import qualified Control.Monad as Mo
import qualified Data.Default as Df
import qualified Data.Map as Mp
import qualified Data.Maybe as Mb
import qualified System.Random as Rn

import qualified Net.Connections as Cn
import qualified Transact.Model.Actions as Ac
import qualified Transact.Model.Common as Co
import qualified Transact.Model.Message as Ms
import qualified Transact.Tablet.Env as En
import qualified Transact.Tablet.TabletInputHandler as TIH
import qualified Transact.Tablet.TabletState as TS
import Transact.Infra.State
import Infra.Lens

transactEIds = [
  Co.EndpointId "172.18.0.3",
  Co.EndpointId "172.18.0.4",
  Co.EndpointId "172.18.0.5",
  Co.EndpointId "172.18.0.6",
  Co.EndpointId "172.18.0.7"]

-- A TransactTabletThread manages a Tablet The Path and
-- KeyRange of the Tablet is specified by PartitionShape.
startTabletThread
  :: Rn.StdGen
  -> Co.TabletShape
  -> Ct.Chan (Ac.T'InputAction)
  -> MV.MVar (Cn.Connections Ms.Message)
  -> IO ()
startTabletThread rg partitionShapes iActionChan connM = do
  print $ "Starting TransactTabletThread with partitionShape: " ++ (show partitionShapes)
  let g = TS.constructor rg
  handleMessage g
  where
    handleMessage :: TS.TabletState -> IO ()
    handleMessage g = do
      iAction <- Ct.readChan iActionChan
      let (_, (oActions, _, g')) = runST (TIH.handleInputAction iAction) g
      conn <- MV.readMVar connM
      Mo.forM_ oActions $ \action ->
        case action of
          Ac.T'Send eIds msg -> Mo.forM_ eIds $
            \eId -> Mp.lookup eId conn & Mb.fromJust $ msg
          Ac.T'Print message -> do
            putStrLn message
      handleMessage g'
