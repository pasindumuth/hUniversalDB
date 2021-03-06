module Slave.Tablet.TabletInputHandler where

import qualified System.Random as Rn

import qualified Infra.Utils as U
import qualified Paxos.MultiPaxosInstance as MP
import qualified Paxos.PaxosLog as PL
import qualified Paxos.Tasks.PaxosTaskManager as PTM
import qualified Paxos.Tasks.Task as Ta
import qualified Proto.Actions.TabletActions as TAc
import qualified Proto.Common as Co
import qualified Proto.Messages as Ms
import qualified Proto.Messages.ClientRequests as CRq
import qualified Proto.Messages.ClientResponses as CRs
import qualified Proto.Messages.ClientResponses.SlaveRead as CRsSR
import qualified Proto.Messages.ClientResponses.SlaveWrite as CRsSW
import qualified Proto.Messages.PaxosMessages as PM
import qualified Proto.Messages.TabletMessages as TM
import qualified Proto.Messages.TraceMessages as TrM
import qualified Slave.Tablet.Env as En
import qualified Slave.Tablet.MultiVersionKVStore as MVS
import qualified Slave.Tablet.DerivedState as DS
import qualified Slave.Tablet.DerivedStateHandler as DSH
import qualified Slave.Tablet.TabletState as TS
import Infra.Lens
import Infra.State

handlingState :: Lens' TS.TabletState (
  MP.MultiPaxosInstance,
  DS.DerivedState,
  PTM.PaxosTaskManager DS.DerivedState TAc.OutputAction TrM.TraceMessage,
  Rn.StdGen,
  [Co.EndpointId])
handlingState =
  (lp5 (
    TS.multiPaxosInstance,
    TS.derivedState,
    TS.paxosTaskManager,
    TS.env.En.rand,
    TS.env.En.slaveEIds))

handleInputAction
  :: TAc.InputAction
  -> STT TS.TabletState ()
handleInputAction iAction = do
  tabletId <- getL TS.tabletId
  case iAction of
    TAc.Receive eId tabletMsg ->
      case tabletMsg of
        TM.ForwardedClientRequest request -> do
          handlingState .^ (PTM.handleTask $ clientTask tabletId eId request)
        TM.MultiPaxosMessage multiPaxosMsg -> do
          pl <- getL $ TS.multiPaxosInstance.MP.paxosLog
          slaveEIds <- getL $ TS.env.En.slaveEIds
          lp2 (TS.multiPaxosInstance, TS.env.En.rand)
            .^ MP.handleMultiPaxos eId slaveEIds multiPaxosMsg (Ms.TabletMessage tabletId . TM.MultiPaxosMessage)
          pl' <- getL $ TS.multiPaxosInstance.MP.paxosLog
          if (pl /= pl')
            then do
              addA $ TAc.Print $ U.ppShow pl'
              paxosId <- getL $ TS.multiPaxosInstance . MP.paxosId
              TS.derivedState .^ DSH.handleDerivedState paxosId pl pl'
              handlingState .^ PTM.handleInsert
            else return ()
    TAc.RetryInput counterValue ->
      handlingState .^ PTM.handleRetry counterValue

clientTask
  :: Co.TabletId
  -> Co.EndpointId
  -> TM.ClientRequest
  -> Ta.Task DS.DerivedState TAc.OutputAction TrM.TraceMessage
clientTask tabletId eId request =
  let description = show (eId, request)
      requestId = request ^. TM.meta.TM.requestId
  in case request ^. TM.payload of
    TM.TabletRead key timestamp ->
      let entry = PM.Tablet $ PM.Read requestId key timestamp
          tryHandling derivedState = do
            case (derivedState ^. DS.kvStore) & MVS.staticReadLat key of
              Just lat | timestamp <= lat -> do
                done derivedState
                return $ Right ()
              _ -> return $ Left entry
          done derivedState = do
            let value = case (derivedState ^. DS.kvStore) &  MVS.staticRead key timestamp of
                          Just ((value, _), _) -> Just value
                          Nothing -> Nothing
                response =
                  CRs.ClientResponse
                    (CRs.Meta requestId)
                    (CRs.SlaveRead
                      (CRsSR.Success value))
            trace $ TrM.ClientResponseSent response
            addA $ TAc.Send [eId] $ Ms.ClientResponse response
          msgWrapper = Ms.TabletMessage tabletId . TM.MultiPaxosMessage
      in Ta.Task description tryHandling done msgWrapper
    TM.TabletWrite key value timestamp ->
      let entry = PM.Tablet $ PM.Write requestId key value timestamp
          tryHandling derivedState = do
            case (derivedState ^. DS.kvStore) & MVS.staticReadLat key of
              Just lat | timestamp <= lat -> do
                let response =
                      case (derivedState ^. DS.kvStore) & MVS.staticRead key timestamp of
                        Just ((_, requestId'), _) | requestId' == requestId ->
                          CRs.ClientResponse
                            (CRs.Meta requestId)
                            (CRs.SlaveWrite CRsSW.Success)
                        _ ->
                          CRs.ClientResponse
                            (CRs.Meta requestId)
                            (CRs.SlaveWrite CRsSW.BackwardsWrite)
                trace $ TrM.ClientResponseSent response
                addA $ TAc.Send [eId] $ Ms.ClientResponse response
                return $ Right ()
              _ -> return $ Left entry
          done derivedState = do
            let response =
                  CRs.ClientResponse
                    (CRs.Meta requestId)
                    (CRs.SlaveWrite CRsSW.Success)
            trace $ TrM.ClientResponseSent response
            addA $ TAc.Send [eId] $ Ms.ClientResponse response
          msgWrapper = Ms.TabletMessage tabletId . TM.MultiPaxosMessage
      in Ta.Task description tryHandling done msgWrapper
