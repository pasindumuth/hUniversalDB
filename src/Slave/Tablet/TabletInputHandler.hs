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
import qualified Proto.Messages.ClientRequests as CRq
import qualified Proto.Messages.ClientResponses as CRs
import qualified Proto.Messages.ClientResponses.SlaveRead as CRsSR
import qualified Proto.Messages.ClientResponses.SlaveWrite as CRsSW
import qualified Proto.Messages.PaxosMessages as PM
import qualified Proto.Messages.TabletMessages as TM
import qualified Proto.Messages.TraceMessages as TrM
import qualified Slave.Tablet.MultiVersionKVStore as MVS
import qualified Slave.Tablet.DerivedState as DS
import qualified Slave.Tablet.TabletState as TS

handlingState :: Lens' TS.TabletState (
  MP.MultiPaxosInstance,
  DS.DerivedState,
  PTM.PaxosTaskManager DS.DerivedState,
  Rn.StdGen,
  [Co.EndpointId])
handlingState =
  (lp5 (
    TS.multiPaxosInstance,
    TS.derivedState,
    TS.paxosTaskManager,
    TS.env.En.rand,
    TS.env.En.slaveEIds))

pastWriteAttempt = "Attempting to write into the past."

handleInputAction
  :: Ac.TabletInputAction
  -> ST TS.TabletState ()
handleInputAction iAction = do
  keySpaceRange <- getL TS.range
  case iAction of
    Ac.TabletReceive eId tabletMsg ->
      case tabletMsg of
        TM.ForwardedClientRequest request -> do
          handlingState .^ (PTM.handleTask $ createClientTask keySpaceRange eId request)
        TM.MultiPaxosMessage multiPaxosMsg -> do
          pl <- getL $ TS.multiPaxosInstance.MP.paxosLog
          slaveEIds <- getL $ TS.env.En.slaveEIds
          lp2 (TS.multiPaxosInstance, TS.env.En.rand)
            .^ MP.handleMultiPaxos eId slaveEIds multiPaxosMsg (Ms.TabletMessage keySpaceRange . TM.MultiPaxosMessage)
          pl' <- getL $ TS.multiPaxosInstance.MP.paxosLog
          if (pl /= pl')
            then do
              addA $ Ac.Print $ ppShow pl'
              paxosId <- getL $ TS.multiPaxosInstance.MP.paxosId
              TS.derivedState .^ DS.handleDerivedState paxosId pl pl'
              handlingState .^ PTM.handleInsert
            else return ()
    Ac.TabletRetryInput counterValue ->
      handlingState .^ PTM.handleRetry counterValue

createClientTask
  :: Co.KeySpaceRange
  -> Co.EndpointId
  -> TM.ClientRequest
  -> Ta.Task DS.DerivedState
createClientTask keySpaceRange eId request =
  let description = show (eId, request)
      requestId = request ^. TM.meta.TM.requestId
  in case request ^. TM.payload of
    TM.TabletRead key timestamp ->
      let description = description
          tryHandling derivedState = do
            case (derivedState ^. DS.kvStore) & MVS.staticReadLat key of
              Just lat | timestamp <= lat -> do
                done derivedState
                return True
              _ -> return False
          done derivedState = do
            let value = case (derivedState ^. DS.kvStore) &  MVS.staticRead key timestamp of
                          Just (value, _, _) -> Just value
                          Nothing -> Nothing
                response =
                  CRs.ClientResponse
                    (CRs.Meta requestId)
                    (CRs.SlaveRead
                      (CRsSR.Success value))
            trace $ TrM.ClientResponseSent response
            addA $ Ac.Send [eId] $ Ms.ClientResponse response
          createPLEntry _ = PM.Tablet $ PM.Read requestId key timestamp
          msgWrapper = Ms.TabletMessage keySpaceRange . TM.MultiPaxosMessage
      in Ta.Task description tryHandling done createPLEntry msgWrapper
    TM.TabletWrite key value timestamp ->
      let description = description
          tryHandling derivedState = do
            case (derivedState ^. DS.kvStore) & MVS.staticReadLat key of
              Just lat | timestamp <= lat -> do
                let response =
                      case (derivedState ^. DS.kvStore) & MVS.staticRead key timestamp of
                        Just (_, requestId', _) | requestId' == requestId ->
                          CRs.ClientResponse
                            (CRs.Meta requestId)
                            (CRs.SlaveWrite CRsSW.Success)
                        _ ->
                          CRs.ClientResponse
                            (CRs.Meta requestId)
                            (CRs.SlaveWrite CRsSW.BackwardsWrite)
                trace $ TrM.ClientResponseSent response
                addA $ Ac.Send [eId] $ Ms.ClientResponse response
                return True
              _ -> return False
          done derivedState = do
            let response =
                  CRs.ClientResponse
                    (CRs.Meta requestId)
                    (CRs.SlaveWrite CRsSW.Success)
            trace $ TrM.ClientResponseSent response
            addA $ Ac.Send [eId] $ Ms.ClientResponse response
          createPLEntry _ = PM.Tablet $ PM.Write requestId key value timestamp
          msgWrapper = Ms.TabletMessage keySpaceRange . TM.MultiPaxosMessage
      in Ta.Task description tryHandling done createPLEntry msgWrapper
