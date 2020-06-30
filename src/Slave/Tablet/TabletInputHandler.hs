module Slave.Tablet.TabletInputHandler where

import qualified Paxos.MultiPaxosInstance as MP
import qualified Paxos.PaxosLog as PL
import qualified Paxos.Tasks.PaxosTaskManager as PTM
import qualified Proto.Actions.Actions as Ac
import qualified Proto.Messages as Ms
import qualified Slave.Tablet.DerivedState as DS
import qualified Slave.Tablet.Env as En
import qualified Slave.Tablet.GlobalState as GS
import Infra.Lens
import Infra.State

import qualified System.Random as Rn

import qualified Paxos.Tasks.Task as Ta
import qualified Proto.Common as Co
import qualified Proto.Messages.ClientMessages as CM
import qualified Proto.Messages.PaxosMessages as PM
import qualified Slave.Tablet.MultiVersionKVStore as MS
import qualified Slave.Tablet.Internal_DerivedState as DS

handlingState :: Lens' GS.GlobalState (
  PL.PaxosLog,
  MP.MultiPaxosInstance,
  DS.DerivedState,
  PTM.PaxosTaskManager DS.DerivedState,
  Rn.StdGen,
  [Co.EndpointId])
handlingState =
  (lp6 (
    GS.paxosLog,
    GS.multiPaxosInstance,
    GS.derivedState,
    GS.paxosTaskManager,
    GS.env.En.rand,
    GS.env.En.slaveEIds))

handleInputAction
  :: Ac.InputAction
  -> ST GS.GlobalState ()
handleInputAction iAction =
  case iAction of
    Ac.Receive eId msg ->
      case msg of
        Ms.ClientRequest request -> handlingState .^ (PTM.handleTask $ createClientTask eId request)
        Ms.MultiPaxosMessage multiPaxosMsg -> do
          pl <- getL GS.paxosLog
          slaveEIds <- getL $ GS.env.En.slaveEIds
          lp3 (GS.multiPaxosInstance, GS.paxosLog, GS.env.En.rand) .^ MP.handleMultiPaxos eId slaveEIds multiPaxosMsg
          pl' <- getL GS.paxosLog
          if (pl /= pl')
            then do
              addA $ Ac.Print $ show pl'
              GS.derivedState .^ DS.handleDerivedState pl pl'
              handlingState .^ PTM.handleInsert
            else return ()
    Ac.RetryInput counterValue ->
      handlingState .^ PTM.handleRetry counterValue

createClientTask :: Co.EndpointId -> CM.ClientRequest -> Ta.Task DS.DerivedState
createClientTask eId request =
  let description = show (eId, request)
  in case request of
    (CM.ReadRequest key timestamp) ->
      let description = description
          tryHandling derivedState = do
            case (derivedState ^. DS.kvStore) & MS.staticReadLat key of
              Just lat | timestamp <= lat -> do
                addA $ Ac.Send [eId] $ Ms.ClientResponse $ CM.ReadResponse $ MS.staticRead key timestamp (derivedState ^. DS.kvStore)
                return True
              _ -> return False
          done derivedState = do
            let value = derivedState ^. DS.kvStore & MS.staticRead key timestamp
            addA $ Ac.Send [eId] $ Ms.ClientResponse $ CM.ReadResponse value
          createPLEntry _ = PM.Read key timestamp
      in Ta.Task description tryHandling done createPLEntry
    (CM.WriteRequest key value timestamp) ->
      let description = description
          tryHandling derivedState = do
            case (derivedState ^. DS.kvStore) & MS.staticReadLat key of
              Just lat | timestamp <= lat -> do
                addA $ Ac.Send [eId] $ Ms.ClientResponse $ CM.Error "Attempting to write into the past."
                return True
              _ -> return False
          done derivedState = do
            addA $ Ac.Send [eId] $ Ms.ClientResponse $ CM.WriteResponse
          createPLEntry _ = PM.Write key value timestamp
      in Ta.Task description tryHandling done createPLEntry
