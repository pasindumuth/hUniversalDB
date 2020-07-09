module Thread.SlaveThread where

import qualified Control.Concurrent as Ct
import qualified Control.Concurrent.MVar as MV
import qualified Control.Monad as Mo
import qualified Data.Default as Df
import qualified Data.Map as Mp
import qualified Data.Maybe as Mb
import qualified System.Random as Rn

import qualified Infra.Utils as U
import qualified Net.Connections as Cn
import qualified Proto.Actions.Actions as Ac
import qualified Proto.Common as Co
import qualified Slave.SlaveInputHandler as SIH
import qualified Slave.Env as En
import qualified Slave.SlaveState as SS
import qualified Thread.TabletThread as TT
import Infra.Lens
import Infra.State

slaveEIds = ["172.18.0.3", "172.18.0.4", "172.18.0.5", "172.18.0.6", "172.18.0.7"]

type TabletMap = Mp.Map Co.KeySpaceRange (Ct.Chan Ac.TabletInputAction)

startSlaveThread
  :: Rn.StdGen
  -> Ct.Chan (Ac.InputAction)
  -> MV.MVar Cn.Connections
  -> IO ()
startSlaveThread rg iActionChan connM = do
  let g = SS.constructor "" rg slaveEIds
      tabletMap = Mp.empty
  handlePaxosMessage g tabletMap
  where
    handlePaxosMessage :: SS.SlaveState -> TabletMap -> IO ()
    handlePaxosMessage g tabletMap = do
      iAction <- Ct.readChan iActionChan
      let (_, (oActions, _, g')) = runST (SIH.handleInputAction iAction) g
      conn <- MV.readMVar connM
      (g'', tabletMap') <- U.s31 Mo.foldM (g', tabletMap) (reverse oActions) $ \(g', tabletMap) action ->
        case action of
          Ac.Send eIds msg -> do
            Mo.forM_ eIds $ \eId -> Mp.lookup eId conn & Mb.fromJust $ msg
            return (g', tabletMap)
          Ac.RetryOutput counterValue -> do
            Ct.forkIO $ do
              Ct.threadDelay 100000
              Ct.writeChan iActionChan $ Ac.RetryInput counterValue
            return (g', tabletMap)
          Ac.Print message -> do
            print message
            return (g', tabletMap)
          Ac.Slave_CreateTablet ranges ->
            U.s31 Mo.foldM (g', tabletMap) ranges $ \(g', tabletMap) range ->
              if tabletMap & Mp.member range
                then return (g', tabletMap)
                else do
                  let (r, rg) = Rn.random $ g' ^. SS.env.En.rand
                      g'' = g' & SS.env.En.rand .~ rg
                      tabletRand = Rn.mkStdGen r
                  tabletIActionChan <- Ct.newChan
                  Ct.forkIO $ TT.startTabletThread tabletRand range tabletIActionChan connM
                  return (g'', tabletMap & Mp.insert range tabletIActionChan)
          Ac.TabletForward range eId msg -> do
            let tabletIActionChan = tabletMap ^?! ix range
            Ct.writeChan tabletIActionChan $ Ac.TabletReceive eId msg
            return (g', tabletMap)
      handlePaxosMessage g'' tabletMap'
