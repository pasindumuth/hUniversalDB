module Slave.SlaveInputHandler where

import qualified Data.List as Li
import qualified Data.Set as St
import qualified Data.Map as Mp
import qualified System.Random as Rn

import qualified Infra.Utils as U
import qualified Paxos.MultiPaxosInstance as MP
import qualified Paxos.PaxosLog as PL
import qualified Paxos.Tasks.PaxosTaskManager as PTM
import qualified Paxos.Tasks.Task as Ta
import qualified Proto.Actions.SlaveActions as SAc
import qualified Proto.Common as Co
import qualified Proto.Messages as Ms
import qualified Proto.Messages.ClientRequests as CRq
import qualified Proto.Messages.ClientResponses as CRs
import qualified Proto.Messages.ClientResponses.RangeWrite as CRsRW
import qualified Proto.Messages.ClientResponses.RangeRead as CRsRR
import qualified Proto.Messages.ClientResponses.SlaveRead as CRsSR
import qualified Proto.Messages.ClientResponses.SlaveWrite as CRsSW
import qualified Proto.Messages.PaxosMessages as PM
import qualified Proto.Messages.SlaveMessages as SM
import qualified Proto.Messages.TabletMessages as TM
import qualified Proto.Messages.TraceMessages as TrM
import qualified Slave.DerivedState as DS
import qualified Slave.Env as En
import qualified Slave.SlaveState as SS
import qualified Slave.DerivedState as DS
import qualified Slave.DerivedStateHandler as DSH
import qualified Slave.KeySpaceManager as KSM
import Infra.Lens
import Infra.State

handlingState :: Lens' SS.SlaveState (
  MP.MultiPaxosInstance,
  DS.DerivedState,
  PTM.PaxosTaskManager DS.DerivedState SAc.OutputAction TrM.TraceMessage,
  Rn.StdGen,
  [Co.EndpointId])
handlingState =
  (lp5 (
    SS.multiPaxosInstance,
    SS.derivedState,
    SS.paxosTaskManager,
    SS.env.En.rand,
    SS.env.En.slaveEIds))

getRangeTIds :: Co.Timestamp -> KSM.KeySpaceManager -> [(Co.KeySpaceRange, Co.TabletId)]
getRangeTIds timestamp keySpaceManager =
  let version = KSM.staticRead timestamp keySpaceManager
  in case version of
    Nothing -> []
    Just (_, _, rangeTIds) -> rangeTIds

handleInputAction
  :: SAc.InputAction
  -> STS SS.SlaveState ()
handleInputAction iAction =
  case iAction of
    SAc.Receive eId msg ->
      case msg of
        Ms.ClientRequest request -> do
          let requestId = request ^. CRq.meta .CRq.requestId
          trace $ TrM.ClientRequestReceived request
          case request ^. CRq.payload of
            CRq.RangeRead timestamp -> do
              let description = show (eId, request)
                  task = rangeReadTask eId requestId timestamp description
              handlingState .^ (PTM.handleTask task)
            CRq.RangeWrite ranges timestamp -> do
              let description = show (eId, request)
              task <- rangeWriteTask eId requestId ranges timestamp description
              handlingState .^ (PTM.handleTask task)
            CRq.SlaveRead path key timestamp -> do
              let description = show (eId, request)
                  task = slaveReadTask eId requestId path key timestamp description
              handlingState .^ (PTM.handleTask task)
            CRq.SlaveWrite path key value timestamp -> do
              let description = show (eId, request)
                  task = slaveWriteTask eId requestId path key value timestamp description
              handlingState .^ (PTM.handleTask task)
            _ -> error $ "ClientRequest " ++ (show request) ++ " is not supported by Slaves."
        Ms.SlaveMessage slaveMsg ->
          case slaveMsg of
            SM.MultiPaxosMessage multiPaxosMsg -> do
              pl <- getL $ SS.multiPaxosInstance.MP.paxosLog
              slaveEIds <- getL $ SS.env.En.slaveEIds
              lp2 (SS.multiPaxosInstance, SS.env.En.rand)
                .^ MP.handleMultiPaxos eId slaveEIds multiPaxosMsg (Ms.SlaveMessage . SM.MultiPaxosMessage)
              pl' <- getL $ SS.multiPaxosInstance.MP.paxosLog
              if (pl /= pl')
                then do
                  addA $ SAc.Print $ U.ppShow pl'
                  paxosId <- getL $ SS.multiPaxosInstance.MP.paxosId
                  SS.derivedState .^ DSH.handleDerivedState paxosId pl pl'
                  handlingState .^ PTM.handleInsert
                else return ()
        Ms.TabletMessage tabletId tabletMsg -> do
          tabletIds <- SS.derivedState . DS.keySpaceManager .^^^ KSM.allRanges
          if St.member tabletId tabletIds
            then addA $ SAc.TabletForward tabletId eId tabletMsg
            else return ()
        _ -> error $ "Message " ++ (show msg) ++ " is not supported by Slaves."
    SAc.RetryInput counterValue ->
      handlingState .^ PTM.handleRetry counterValue

rangeReadTask
  :: Co.EndpointId
  -> Co.RequestId
  -> Co.Timestamp
  -> String
  -> Ta.Task DS.DerivedState SAc.OutputAction TrM.TraceMessage
rangeReadTask eId requestId timestamp description =
  let entry = PM.Slave $ PM.RangeRead requestId timestamp
      tryHandling derivedState = do
        let lat = KSM.staticReadLat $ derivedState ^. DS.keySpaceManager
        if lat < timestamp
          then return $ Left entry
          else do
            done derivedState
            return $ Right ()
      done derivedState = do
        let rangeTIds = getRangeTIds timestamp $ derivedState ^. DS.keySpaceManager
            response =
              CRs.ClientResponse
                (CRs.Meta requestId)
                (CRs.RangeRead
                  (CRsRR.Success (map fst rangeTIds)))
        trace $ TrM.ClientResponseSent response
        addA $ SAc.Send [eId] $ Ms.ClientResponse response
      msgWrapper = Ms.SlaveMessage . SM.MultiPaxosMessage
  in Ta.Task description tryHandling done msgWrapper

rangeWriteTask
  :: Co.EndpointId
  -> Co.RequestId
  -> [Co.KeySpaceRange]
  -> Co.Timestamp
  -> String
  -> STS SS.SlaveState (Ta.Task DS.DerivedState SAc.OutputAction TrM.TraceMessage)
rangeWriteTask eId requestId ranges timestamp description = do
  r <- SS.env . En.rand .^^ Rn.random
  let rand = Rn.mkStdGen r
      tryHandling derivedState = do
        let lat = KSM.staticReadLat $ derivedState ^. DS.keySpaceManager
        if lat < timestamp
          then do
            let oldRangeTIds = getRangeTIds lat $ derivedState ^. DS.keySpaceManager
                oldRangeTIdMap = Mp.fromList oldRangeTIds
                (newRangeTIds, rand') = U.s31 foldl ([], rand) ranges $
                  \(newRangeTIds, rand) range ->
                    case Mp.lookup range oldRangeTIdMap of
                      Just tabletId -> ((range, tabletId):newRangeTIds, rand)
                      Nothing ->
                        let (tabletId, rand') = U.mkUID rand & _1 %~ Co.PaxosId & _1 %~ Co.TabletId
                        in ((range, tabletId):newRangeTIds, rand')
            return $ Left $ PM.Slave $ PM.RangeWrite requestId timestamp newRangeTIds
          else do
            let response =
                  case (KSM.staticRead timestamp $ derivedState ^. DS.keySpaceManager) of
                    Just (_, requestId', _) | requestId' == requestId ->
                      CRs.ClientResponse
                        (CRs.Meta requestId)
                        (CRs.RangeWrite CRsRW.Success)
                    _ ->
                      CRs.ClientResponse
                        (CRs.Meta requestId)
                        (CRs.RangeWrite CRsRW.BackwardsWrite)
            trace $ TrM.ClientResponseSent response
            addA $ SAc.Send [eId] $ Ms.ClientResponse response
            return $ Right ()
      done _ = do
        let response =
              CRs.ClientResponse
                (CRs.Meta requestId)
                (CRs.RangeWrite CRsRW.Success)
        trace $ TrM.ClientResponseSent response
        addA $ SAc.Send [eId] $ Ms.ClientResponse response
      msgWrapper = Ms.SlaveMessage . SM.MultiPaxosMessage
  return $ Ta.Task description tryHandling done msgWrapper

slaveReadTask
  :: Co.EndpointId
  -> Co.RequestId
  -> Co.Path
  -> Co.Key
  -> Co.Timestamp
  -> String
  -> Ta.Task DS.DerivedState SAc.OutputAction TrM.TraceMessage
slaveReadTask eId requestId path key timestamp description =
  let entry = PM.Slave $ PM.RangeRead requestId timestamp
      tryHandling derivedState = do
        let lat = KSM.staticReadLat $ derivedState ^. DS.keySpaceManager
        if lat < timestamp
          then return $ Left entry
          else do
            done derivedState
            return $ Right ()
      done derivedState = do
        let rangeTIds = getRangeTIds timestamp $ derivedState ^. DS.keySpaceManager
            range = Co.KeySpaceRange path
        case Li.find (\(range', _) -> range' == range) rangeTIds of
          Just (_, tabletId) -> do
            addA $ SAc.TabletForward tabletId eId $ TM.ForwardedClientRequest $
              TM.ClientRequest
                (TM.Meta requestId)
                (TM.TabletRead key timestamp)
          Nothing -> do
            let response =
                  CRs.ClientResponse
                    (CRs.Meta requestId)
                    (CRs.SlaveRead CRsSR.UnknownDB)
            trace $ TrM.ClientResponseSent response
            addA $ SAc.Send [eId] $ Ms.ClientResponse response
      msgWrapper = Ms.SlaveMessage . SM.MultiPaxosMessage
  in Ta.Task description tryHandling done msgWrapper

slaveWriteTask
  :: Co.EndpointId
  -> Co.RequestId
  -> Co.Path
  -> Co.Key
  -> Co.Value
  -> Co.Timestamp
  -> String
  -> Ta.Task DS.DerivedState SAc.OutputAction TrM.TraceMessage
slaveWriteTask eId requestId path key value timestamp description =
  let entry = PM.Slave $ PM.RangeRead requestId timestamp
      tryHandling derivedState = do
        let lat = KSM.staticReadLat $ derivedState ^. DS.keySpaceManager
        if lat < timestamp
          then return $ Left entry
          else do
            done derivedState
            return $ Right ()
      done derivedState = do
        let rangeTIds = getRangeTIds timestamp $ derivedState ^. DS.keySpaceManager
            range = Co.KeySpaceRange path
        case Li.find (\(range', _) -> range' == range) rangeTIds of
          Just (_, tabletId) -> do
            addA $ SAc.TabletForward tabletId eId $ TM.ForwardedClientRequest $
              TM.ClientRequest
                (TM.Meta requestId)
                (TM.TabletWrite key value timestamp)
          Nothing -> do
            let response =
                  CRs.ClientResponse
                    (CRs.Meta requestId)
                    (CRs.SlaveWrite CRsSW.UnknownDB)
            trace $ TrM.ClientResponseSent response
            addA $ SAc.Send [eId] $ Ms.ClientResponse response
      msgWrapper = Ms.SlaveMessage . SM.MultiPaxosMessage
  in Ta.Task description tryHandling done msgWrapper
