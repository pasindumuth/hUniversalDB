module Slave.SlaveInputHandler where

import qualified Data.List as Li
import qualified Data.Set as St
import qualified System.Random as Rn

import qualified Infra.Utils as U
import qualified Paxos.MultiPaxosInstance as MP
import qualified Paxos.PaxosLog as PL
import qualified Paxos.Tasks.PaxosTaskManager as PTM
import qualified Paxos.Tasks.Task as Ta
import qualified Proto.Actions.Actions as Ac
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
import qualified Slave.Internal_DerivedState as IDS
import qualified Slave.KeySpaceManager as KSM
import Infra.Lens
import Infra.State

handlingState :: Lens' SS.SlaveState (
  MP.MultiPaxosInstance,
  DS.DerivedState,
  PTM.PaxosTaskManager DS.DerivedState,
  Rn.StdGen,
  [Co.EndpointId])
handlingState =
  (lp5 (
    SS.multiPaxosInstance,
    SS.derivedState,
    SS.paxosTaskManager,
    SS.env.En.rand,
    SS.env.En.slaveEIds))

getRanges :: Co.Timestamp -> KSM.KeySpaceManager -> [Co.KeySpaceRange]
getRanges timestamp keySpaceManager =
  let version = KSM.staticRead timestamp keySpaceManager
  in case version of
    Nothing -> []
    Just (_, _, ranges) -> ranges

handleInputAction
  :: Ac.InputAction
  -> ST SS.SlaveState ()
handleInputAction iAction =
  case iAction of
    Ac.Receive eId msg ->
      case msg of
        Ms.ClientRequest request -> do
          -- TODO: take a hash of the request and use that as the ID instead to guarantee one-way ness.
          let requestId = (request ^. CRq.meta . CRq.requestId)
          trace $ TrM.ClientRequestReceived request
          case request ^. CRq.payload of
            CRq.RangeRead timestamp -> do
              let description = show (eId, request)
                  task = createRangeReadTask eId requestId timestamp description
              handlingState .^ (PTM.handleTask task)
            CRq.RangeWrite ranges timestamp -> do
              let description = show (eId, request)
                  task = createRangeWriteTask eId requestId ranges timestamp description
              handlingState .^ (PTM.handleTask task)
            CRq.SlaveRead databaseId tableId key timestamp -> do
              let description = show (eId, request)
                  task = createSlaveReadTask eId requestId (databaseId, tableId) key timestamp description
              handlingState .^ (PTM.handleTask task)
            CRq.SlaveWrite databaseId tableId key value timestamp -> do
              let description = show (eId, request)
                  task = createSlaveWriteTask eId requestId (databaseId, tableId) key value timestamp description
              handlingState .^ (PTM.handleTask task)
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
                  addA $ Ac.Print $ show pl'
                  paxosId <- getL $ SS.multiPaxosInstance.MP.paxosId
                  SS.derivedState .^ DS.handleDerivedState paxosId pl pl'
                  handlingState .^ PTM.handleInsert
                else return ()
        Ms.TabletMessage keySpaceRange tabletMsg -> do
          ranges <- SS.derivedState.IDS.keySpaceManager .^^^ KSM.allRanges
          if St.member keySpaceRange ranges
            then addA $ Ac.TabletForward keySpaceRange eId tabletMsg
            else return ()
        Ms.ClientResponse _ -> U.caseError
    Ac.RetryInput counterValue ->
      handlingState .^ PTM.handleRetry counterValue

createRangeReadTask
  :: Co.EndpointId
  -> Co.RequestId
  -> Co.Timestamp
  -> String
  -> Ta.Task DS.DerivedState
createRangeReadTask eId requestId timestamp description =
  let tryHandling derivedState = do
        let lat = KSM.staticReadLat $ derivedState ^. IDS.keySpaceManager
        if lat < timestamp
          then return False
          else do
            done derivedState
            return True
      done derivedState = do
        let ranges = getRanges timestamp $ derivedState ^. IDS.keySpaceManager
            response =
              CRs.ClientResponse
                (CRs.Meta requestId)
                (CRs.RangeRead
                  (CRsRR.Success ranges))
        trace $ TrM.ClientResponseSent response
        addA $ Ac.Send [eId] $ Ms.ClientResponse response
      createPLEntry derivedState =
        PM.Slave $ PM.RangeRead requestId timestamp
      msgWrapper = Ms.SlaveMessage . SM.MultiPaxosMessage
  in Ta.Task description tryHandling done createPLEntry msgWrapper

createRangeWriteTask
  :: Co.EndpointId
  -> Co.RequestId
  -> [Co.KeySpaceRange]
  -> Co.Timestamp
  -> String
  -> Ta.Task DS.DerivedState
createRangeWriteTask eId requestId ranges timestamp description =
  let tryHandling derivedState = do
        let lat = KSM.staticReadLat $ derivedState ^. IDS.keySpaceManager
        if lat < timestamp
          then return False
          else do
            let response =
                  case (KSM.staticRead timestamp $ derivedState ^. IDS.keySpaceManager) of
                    Just (_, requestId', _) | requestId' == requestId ->
                      CRs.ClientResponse
                        (CRs.Meta requestId)
                        (CRs.RangeWrite CRsRW.Success)
                    _ ->
                      CRs.ClientResponse
                        (CRs.Meta requestId)
                        (CRs.RangeWrite CRsRW.BackwardsWrite)
            trace $ TrM.ClientResponseSent response
            addA $ Ac.Send [eId] $ Ms.ClientResponse response
            return True
      done _ = do
        let response =
              CRs.ClientResponse
                (CRs.Meta requestId)
                (CRs.RangeWrite CRsRW.Success)
        trace $ TrM.ClientResponseSent response
        addA $ Ac.Send [eId] $ Ms.ClientResponse response
      createPLEntry derivedState = PM.Slave $ PM.RangeWrite requestId timestamp ranges
      msgWrapper = Ms.SlaveMessage . SM.MultiPaxosMessage
  in Ta.Task description tryHandling done createPLEntry msgWrapper

createSlaveReadTask
  :: Co.EndpointId
  -> Co.RequestId
  -> (Co.DatabaseId, Co.TableId)
  -> Co.Key
  -> Co.Timestamp
  -> String
  -> Ta.Task DS.DerivedState
createSlaveReadTask eId requestId (databaseId, tableId) key timestamp description =
  let tryHandling derivedState = do
        let lat = KSM.staticReadLat $ derivedState ^. IDS.keySpaceManager
        if lat < timestamp
          then return False
          else do
            done derivedState
            return True
      done derivedState = do
        let ranges = getRanges timestamp $ derivedState ^. IDS.keySpaceManager
            range = Co.KeySpaceRange databaseId tableId
        if elem range ranges
          then addA $ Ac.TabletForward range eId $ TM.ForwardedClientRequest $
                 TM.ClientRequest
                   (TM.Meta requestId)
                   (TM.TabletRead key timestamp)
          else do
            let response =
                  CRs.ClientResponse
                    (CRs.Meta requestId)
                    (CRs.SlaveRead CRsSR.UnknownDB)
            trace $ TrM.ClientResponseSent response
            addA $ Ac.Send [eId] $ Ms.ClientResponse response
      createPLEntry derivedState =
        PM.Slave $ PM.RangeRead requestId timestamp
      msgWrapper = Ms.SlaveMessage . SM.MultiPaxosMessage
  in Ta.Task description tryHandling done createPLEntry msgWrapper

createSlaveWriteTask
  :: Co.EndpointId
  -> Co.RequestId
  -> (Co.DatabaseId, Co.TableId)
  -> Co.Key
  -> Co.Value
  -> Co.Timestamp
  -> String
  -> Ta.Task DS.DerivedState
createSlaveWriteTask eId requestId (databaseId, tableId) key value timestamp description =
  let tryHandling derivedState = do
        let lat = KSM.staticReadLat $ derivedState ^. IDS.keySpaceManager
        if lat < timestamp
          then return False
          else do
            done derivedState
            return True
      done derivedState = do
        let ranges = getRanges timestamp $ derivedState ^. IDS.keySpaceManager
            range = Co.KeySpaceRange databaseId tableId
        if elem range ranges
          then addA $ Ac.TabletForward range eId $ TM.ForwardedClientRequest $
                 TM.ClientRequest
                   (TM.Meta requestId)
                   (TM.TabletWrite key value timestamp)
          else do
            let response =
                  CRs.ClientResponse
                    (CRs.Meta requestId)
                    (CRs.SlaveWrite CRsSW.UnknownDB)
            trace $ TrM.ClientResponseSent response
            addA $ Ac.Send [eId] $ Ms.ClientResponse response
      createPLEntry derivedState =
        PM.Slave $ PM.RangeRead requestId timestamp
      msgWrapper = Ms.SlaveMessage . SM.MultiPaxosMessage
  in Ta.Task description tryHandling done createPLEntry msgWrapper
