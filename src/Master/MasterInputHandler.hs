module Master.MasterInputHandler where

import qualified Data.Map as Mp
import qualified Data.Maybe as Mb
import qualified System.Random as Rn

import qualified Infra.Utils as U
import qualified Master.DerivedState as DS
import qualified Master.Env as En
import qualified Master.MasterState as MS
import qualified Master.SlaveGroupRanges as SGR
import qualified Master.NetworkTaskManager as NTM
import qualified Paxos.MultiPaxosInstance as MP
import qualified Paxos.PaxosLog as PL
import qualified Paxos.Tasks.PaxosTaskManager as PTM
import qualified Paxos.Tasks.Task as Ta
import qualified Proto.Actions.MasterActions as MAc
import qualified Proto.Common as Co
import qualified Proto.Messages as Ms
import qualified Proto.Messages.ClientRequests as CRq
import qualified Proto.Messages.ClientResponses as CRs
import qualified Proto.Messages.ClientResponses.CreateDatabase as CRsCD
import qualified Proto.Messages.ClientResponses.RangeWrite as CRsRW
import qualified Proto.Messages.PaxosMessages as PM
import qualified Proto.Messages.MasterMessages as MM
import qualified Proto.Messages.TraceMessages as TrM
import Infra.Lens
import Infra.State

handlingState :: Lens' MS.MasterState (
  MP.MultiPaxosInstance,
  DS.DerivedState,
  PTM.PaxosTaskManager DS.DerivedState MAc.OutputAction,
  Rn.StdGen,
  [Co.EndpointId])
handlingState =
  (lp5 (
    MS.multiPaxosInstance,
    MS.derivedState,
    MS.paxosTaskManager,
    MS.env.En.rand,
    MS.env.En.masterEIds))

handleInputAction
  :: MAc.InputAction
  -> STM MS.MasterState ()
handleInputAction iAction =
  case iAction of
    MAc.Receive eId msg ->
      case msg of
        Ms.ClientRequest request -> do
          let requestId = request ^. CRq.meta . CRq.requestId
          trace $ TrM.ClientRequestReceived request
          case request ^. CRq.payload of
            CRq.CreateDatabase databaseId tableId timestamp -> do
              uid <- MS.env . En.rand .^^ U.mkUID
              let description = show (eId, request)
                  task = createDatabaseTask eId requestId databaseId tableId timestamp description uid
              handlingState .^ (PTM.handleTask task)
            _ -> U.caseError
        Ms.MasterMessage masterMsg ->
          case masterMsg of
            MM.MultiPaxosMessage multiPaxosMsg -> do
              pl <- getL $ MS.multiPaxosInstance.MP.paxosLog
              masterEIds <- getL $ MS.env.En.masterEIds
              lp2 (MS.multiPaxosInstance, MS.env.En.rand)
                .^ MP.handleMultiPaxos eId masterEIds multiPaxosMsg (Ms.MasterMessage . MM.MultiPaxosMessage)
              pl' <- getL $ MS.multiPaxosInstance.MP.paxosLog
              if (pl /= pl')
                then do
                  addA $ MAc.Print $ ppShow pl'
                  paxosId <- getL $ MS.multiPaxosInstance.MP.paxosId
                  MS.derivedState .^ DS.handleDerivedState paxosId pl pl'
                  handlingState .^ PTM.handleInsert
                else return ()
        Ms.ClientResponse response -> do
          case response ^. CRs.payload of
            CRs.RangeWrite rangeWrite -> do
              let uid = response ^. CRs.meta . CRs.requestId
              taskMap <- getL $ MS.derivedState . DS.networkTaskManager . NTM.taskMap
              case Mp.lookup uid taskMap of
                Just task ->
                  case task of
                    NTM.CreateDatabase eId requestId timestamp slaveGroupId -> do
                      let description = show (eId, response)
                          choice =
                            case rangeWrite of
                              CRsRW.Success -> Co.NewChoice
                              _ -> Co.OldChoice
                          task = createPickKeySpace requestId eId slaveGroupId timestamp choice uid description
                      return ()
                _ -> return ()
            _ -> U.caseError
        _ -> U.caseError
    MAc.RetryInput counterValue ->
      handlingState .^ PTM.handleRetry counterValue
    MAc.PerformInput uid -> do
      (taskManager, slaveGroupRanges, slaveEIds) <- getL $
        MS.derivedState . (lp3 (
          DS.networkTaskManager,
          DS.slaveGroupRanges,
          DS.slaveEIds))
      lp0 .^ NTM.performTask uid taskManager slaveGroupRanges slaveEIds
      return ()

createDatabaseTask
  :: Co.EndpointId
  -> Co.RequestId
  -> Co.DatabaseId
  -> Co.TableId
  -> Co.Timestamp
  -> String
  -> Co.UID
  -> Ta.Task DS.DerivedState MAc.OutputAction
createDatabaseTask eId requestId databaseId tableId timestamp description uid =
  let keySpaceRange = Co.KeySpaceRange databaseId tableId
      sendResponse responseValue = do
        let response =
              CRs.ClientResponse
                (CRs.Meta requestId)
                (CRs.CreateDatabase responseValue)
        trace $ TrM.ClientResponseSent response
        addA $ MAc.Send [eId] $ Ms.ClientResponse response
      tryHandling derivedState = do
        let lat = SGR.staticReadLat $ derivedState ^. DS.slaveGroupRanges
        if timestamp <= lat
          then do
            sendResponse CRsCD.BackwardsWrite
            return True
          else do
            let latestValues = SGR.staticReadAll lat (derivedState ^. DS.slaveGroupRanges)
                exists = DS.rangeExists keySpaceRange latestValues
            if Mb.isJust exists
              -- We return False so that the entry the PL entry can be inserted, which
              -- will then update the lat before sending back AlreadyExists.
              then return False
              else do
                let maybeExists =
                      U.s31 Mp.foldl False latestValues $ \maybeExists value ->
                        if maybeExists
                          then maybeExists
                          else
                            case value of
                              Just (_, SGR.Changing changingKeySpace) ->
                                elem keySpaceRange (changingKeySpace ^. Co.oldKeySpace) ||
                                elem keySpaceRange (changingKeySpace ^. Co.newKeySpace)
                              _ -> False
                    freeGroupM = DS.findFreeGroupM latestValues
                if maybeExists || Mb.isNothing freeGroupM
                  then do
                    sendResponse CRsCD.NothingChanged
                    return True
                  else return False
      done derivedState = do
        let lat = SGR.staticReadLat $ derivedState ^. DS.slaveGroupRanges
            latestValues = SGR.staticReadAll lat (derivedState ^. DS.slaveGroupRanges)
            exists = DS.rangeExists keySpaceRange latestValues
        if Mb.isJust exists
          then sendResponse CRsCD.AlreadyExists
          else do
            -- The only reason we can be here is if we can go ahead and finish the creation
            let freeGroupM = DS.findFreeGroupM latestValues
            case freeGroupM of
              Just slaveGroupId -> do
                NTM.performTask uid
                  (derivedState ^. DS.networkTaskManager)
                  (derivedState ^. DS.slaveGroupRanges)
                  (derivedState ^. DS.slaveEIds)
                return ()
              Nothing -> U.caseError
      createPLEntry derivedState =
        PM.Master $ PM.CreateDatabase requestId databaseId tableId timestamp eId uid
      msgWrapper = Ms.MasterMessage . MM.MultiPaxosMessage
  in Ta.Task description tryHandling done createPLEntry msgWrapper

createPickKeySpace
  :: Co.SlaveGroupId
  -> Co.RequestId
  -> Co.EndpointId
  -> Co.Timestamp
  -> Co.Choice
  -> Co.UID
  -> String
  -> Ta.Task DS.DerivedState MAc.OutputAction
createPickKeySpace requestId eId slaveGroupId timestamp choice uid description =
  let tryHandling derivedState = do
        case derivedState ^. DS.slaveGroupRanges & SGR.staticRead slaveGroupId timestamp of
          Just (_, SGR.Changing _) -> return False
          _ -> return True
      done derivedState = do
        let responseValue =
              case choice of
                Co.NewChoice -> CRsCD.Success
                Co.OldChoice -> CRsCD.BackwardsWrite
            response =
              CRs.ClientResponse
                (CRs.Meta requestId)
                (CRs.CreateDatabase responseValue)
        trace $ TrM.ClientResponseSent response
        addA $ MAc.Send [eId] $ Ms.ClientResponse response
        return ()
      createPLEntry derivedState =
        PM.Master $ PM.PickKeySpace slaveGroupId choice uid
      msgWrapper = Ms.MasterMessage . MM.MultiPaxosMessage
  in Ta.Task description tryHandling done createPLEntry msgWrapper
