module Transact.Thread.ServerThread where

import qualified Control.Concurrent as Ct
import qualified Control.Concurrent.MVar as MV
import qualified Control.Monad as Mo
import qualified Data.Default as Df
import qualified Data.Map as Mp
import qualified Data.Maybe as Mb
import qualified System.Random as Rn

import qualified Common.Model.RelationalTablet as RT
import qualified Infra.Utils as U
import qualified Net.Connections as Cn
import qualified Transact.Model.Actions as Ac
import qualified Transact.Model.Common as Co
import qualified Transact.Model.Message as Ms
import qualified Transact.Server.ServerInputHandler as TIH
import qualified Transact.Server.ServerState as TS
import Transact.Infra.State
import Infra.Lens

transactEIds = [
  Co.EndpointId "172.18.1.3",
  Co.EndpointId "172.18.1.4",
  Co.EndpointId "172.18.1.5",
  Co.EndpointId "172.18.1.6",
  Co.EndpointId "172.18.1.7"]

-- The keys are the chans used to forward messages to the Transact Tablets.
type TabletMap = Mp.Map Co.TabletShape (Ct.Chan Ac.T'InputAction)

startServerThread
  :: Rn.StdGen
  -> [(RT.Schema, Co.TabletShape)]
  -> TabletMap
  -> Ct.Chan (Ac.S'InputAction)
  -> MV.MVar (Cn.Connections Ms.Message)
  -> IO ()
startServerThread rg shapesWithSchema tabletMap iActionChan connM = do
  print $ "Starting TransactServerThread with partitionShape: " ++ (show shapesWithSchema)
  let g = TS.constructor rg shapesWithSchema
  handleMessage g
  where
    handleMessage :: TS.ServerState -> IO ()
    handleMessage g = do
      iAction <- Ct.readChan iActionChan
      let (_, (oActions, _, g')) = runST (TIH.handleInputAction iAction) g
      conn <- MV.readMVar connM
      Mo.forM_ oActions $ \action ->
        case action of
          Ac.S'Send eIds msg -> do
            Mo.forM_ eIds $ \eId -> Mp.lookup eId conn & Mb.fromJust $ msg
          Ac.S'Print message -> do
            putStrLn message
            return ()
          Ac.S'TabletForward tabletId eId msg -> do
            let tabletIActionChan = tabletMap ^?! ix tabletId
            Ct.writeChan tabletIActionChan $ Ac.T'Receive eId msg
            return ()
      handleMessage g'
