module Slave.Tablet.TabletInputHandler where

import qualified Paxos.MultiPaxosInstance as MP
import qualified Paxos.PaxosLog as PL
import qualified Paxos.Tasks.PaxosTaskManager as PTM
import qualified Proto.Actions.Actions as Ac
import qualified Proto.Messages as Ms
import qualified Slave.Tablet.DerivedState as DS
import qualified Slave.Tablet.Env as En
import qualified Slave.Tablet.TabletState as TS
import Infra.Lens
import Infra.State

import qualified System.Random as Rn

import qualified Paxos.Tasks.Task as Ta
import qualified Proto.Common as Co
import qualified Proto.Messages.ClientMessages as CM
import qualified Proto.Messages.PaxosMessages as PM
import qualified Proto.Messages.TabletMessages as TM
import qualified Slave.Tablet.MultiVersionKVStore as MS
import qualified Slave.Tablet.Internal_DerivedState as DS
import qualified Slave.Tablet.TabletState as TS

handlingState :: Lens' TS.TabletState (
  PL.PaxosLog,
  MP.MultiPaxosInstance,
  DS.DerivedState,
  PTM.PaxosTaskManager DS.DerivedState,
  Rn.StdGen,
  [Co.EndpointId])
handlingState =
  (lp6 (
    TS.paxosLog,
    TS.multiPaxosInstance,
    TS.derivedState,
    TS.paxosTaskManager,
    TS.env.En.rand,
    TS.env.En.slaveEIds))

handleInputAction
  :: Ac.InputAction
  -> ST TS.TabletState ()
handleInputAction iAction =
  case iAction of
    Ac.Receive eId msg ->
      case msg of
        Ms.ClientRequest request -> do
          keySpaceRange <- getL TS.range
          handlingState .^ (PTM.handleTask $ createClientTask keySpaceRange eId request)
        Ms.TabletMessage keySpaceRange tabletMsg ->
          case tabletMsg of
            TM.MultiPaxosMessage multiPaxosMsg -> do
              pl <- getL TS.paxosLog
              slaveEIds <- getL $ TS.env.En.slaveEIds
              lp3 (TS.multiPaxosInstance, TS.paxosLog, TS.env.En.rand)
                .^ MP.handleMultiPaxos eId slaveEIds multiPaxosMsg (Ms.TabletMessage keySpaceRange . TM.MultiPaxosMessage)
              pl' <- getL TS.paxosLog
              if (pl /= pl')
                then do
                  addA $ Ac.Print $ show pl'
                  TS.derivedState .^ DS.handleDerivedState pl pl'
                  handlingState .^ PTM.handleInsert
                else return ()
    Ac.RetryInput counterValue ->
      handlingState .^ PTM.handleRetry counterValue

createClientTask :: Co.KeySpaceRange -> Co.EndpointId -> CM.ClientRequest -> Ta.Task DS.DerivedState
createClientTask keySpaceRange eId request =
  let description = show (eId, request)
  in case request of
    (CM.ReadRequest _ _ key timestamp) ->
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
          createPLEntry _ = PM.Tablet_Read key timestamp
          msgWrapper = Ms.TabletMessage keySpaceRange . TM.MultiPaxosMessage
      in Ta.Task description tryHandling done createPLEntry msgWrapper
    (CM.WriteRequest _ _ key value timestamp) ->
      let description = description
          tryHandling derivedState = do
            case (derivedState ^. DS.kvStore) & MS.staticReadLat key of
              Just lat | timestamp <= lat -> do
                addA $ Ac.Send [eId] $ Ms.ClientResponse $ CM.Error "Attempting to write into the past."
                return True
              _ -> return False
          done derivedState = do
            addA $ Ac.Send [eId] $ Ms.ClientResponse $ CM.WriteResponse
          createPLEntry _ = PM.Tablet_Write key value timestamp
          msgWrapper = Ms.TabletMessage keySpaceRange . TM.MultiPaxosMessage
      in Ta.Task description tryHandling done createPLEntry msgWrapper
