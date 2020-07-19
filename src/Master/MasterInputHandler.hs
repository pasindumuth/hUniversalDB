module Master.MasterInputHandler where

import qualified Data.Map as Mp
import qualified Data.Maybe as Mb
import qualified System.Random as Rn

import qualified Infra.Utils as U
import qualified Master.DerivedState as DS
import qualified Master.DerivedStateHandler as DSH
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
import qualified Proto.Messages.ClientResponses.DeleteDatabase as CRsDD
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
            CRq.DeleteDatabase databaseId tableId timestamp -> do
              uid <- MS.env . En.rand .^^ U.mkUID
              let description = show (eId, request)
                  task = deleteDatabaseTask eId requestId databaseId tableId timestamp description uid
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
                  MS.derivedState .^ DSH.handleDerivedState paxosId pl pl'
                  handlingState .^ PTM.handleInsert
                else return ()
        Ms.ClientResponse response -> do
          case response ^. CRs.payload of
            CRs.RangeWrite rangeWrite -> do
              let uid = Co.UID $ Co.getRequestId $ response ^. CRs.meta . CRs.requestId
              taskMap <- getL $ MS.derivedState . DS.networkTaskManager . NTM.taskMap
              case Mp.lookup uid taskMap of
                Just task -> do
                  let choice =
                        case rangeWrite of
                          CRsRW.Success -> Co.NewChoice
                          _ -> Co.OldChoice
                  case task of
                    NTM.CreateDatabase eId requestId timestamp slaveGroupId -> do
                      let description = show (eId, response)
                          response =
                            CRs.ClientResponse
                              (CRs.Meta requestId)
                              (CRs.CreateDatabase (
                                case choice of
                                  Co.NewChoice -> CRsCD.Success
                                  Co.OldChoice -> CRsCD.BackwardsWrite))
                          task = createPickKeySpace response eId slaveGroupId timestamp choice uid description
                      handlingState .^ (PTM.handleTask task)
                      return ()
                    NTM.DeleteDatabase eId requestId timestamp slaveGroupId -> do
                      let description = show (eId, response)
                          response =
                            CRs.ClientResponse
                              (CRs.Meta requestId)
                              (CRs.DeleteDatabase (
                                case choice of
                                  Co.NewChoice -> CRsDD.Success
                                  Co.OldChoice -> CRsDD.BackwardsWrite))
                          task = createPickKeySpace response eId slaveGroupId timestamp choice uid description
                      handlingState .^ (PTM.handleTask task)
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
          DS.slaveGroupEIds))
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
  let entry = PM.Master $ PM.CreateDatabase requestId databaseId tableId timestamp eId uid
      keySpaceRange = Co.KeySpaceRange databaseId tableId
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
            return $ Right ()
          else do
            let latestValues = SGR.staticReadAll lat (derivedState ^. DS.slaveGroupRanges)
                exists = DSH.rangeExists keySpaceRange latestValues
            if Mb.isJust exists
              -- We return False so that the entry the PL entry can be inserted, which
              -- will then update the lat before sending back AlreadyExists.
              then return $ Left entry
              else do
                let maybeExists = DSH.rangeMaybeExists keySpaceRange latestValues
                    freeGroupM = DSH.findFreeGroupM latestValues
                if maybeExists || Mb.isNothing freeGroupM
                  then do
                    -- In this case, we can't fullfill the request, so respond with an error.
                    sendResponse CRsCD.NothingChanged
                    return $ Right ()
                  else return $ Left entry
      done derivedState = do
        let lat = SGR.staticReadLat $ derivedState ^. DS.slaveGroupRanges
            latestValues = SGR.staticReadAll lat (derivedState ^. DS.slaveGroupRanges)
            exists = DSH.rangeExists keySpaceRange latestValues
        if Mb.isJust exists
          then sendResponse CRsCD.AlreadyExists
          else
            -- The only reason we can be here is if we can go ahead and finish the creation.
            NTM.performTask uid
              (derivedState ^. DS.networkTaskManager)
              (derivedState ^. DS.slaveGroupRanges)
              (derivedState ^. DS.slaveGroupEIds)
      msgWrapper = Ms.MasterMessage . MM.MultiPaxosMessage
  in Ta.Task description tryHandling done msgWrapper

deleteDatabaseTask
  :: Co.EndpointId
  -> Co.RequestId
  -> Co.DatabaseId
  -> Co.TableId
  -> Co.Timestamp
  -> String
  -> Co.UID
  -> Ta.Task DS.DerivedState MAc.OutputAction
deleteDatabaseTask eId requestId databaseId tableId timestamp description uid =
  let entry = PM.Master $ PM.DeleteDatabase requestId databaseId tableId timestamp eId uid
      keySpaceRange = Co.KeySpaceRange databaseId tableId
      sendResponse responseValue = do
        let response =
              CRs.ClientResponse
                (CRs.Meta requestId)
                (CRs.DeleteDatabase responseValue)
        trace $ TrM.ClientResponseSent response
        addA $ MAc.Send [eId] $ Ms.ClientResponse response
      tryHandling derivedState = do
        let lat = SGR.staticReadLat $ derivedState ^. DS.slaveGroupRanges
        if timestamp <= lat
          then do
            sendResponse CRsDD.BackwardsWrite
            return $ Right ()
          else do
            let latestValues = SGR.staticReadAll lat (derivedState ^. DS.slaveGroupRanges)
                exists = DSH.rangeExists keySpaceRange latestValues
            if Mb.isJust exists
              -- We return False, since this is the only valid case for deleting a KeySpaceRange.
              then return $ Left entry
              else
                if DSH.rangeMaybeExists keySpaceRange latestValues
                  then do
                    -- In this case, we can't fullfill the request due to uncertainty,
                    -- so respond with an error.
                    sendResponse CRsDD.NothingChanged
                    return $ Right ()
                  -- In this case, the KeySpaceRange doesn't exist or have potential for exists,
                  -- so w return False so that the entry the PL entry can be inserted, which
                  -- will then update the lat before sending back DoesNotExist.
                  else return $ Left entry
      done derivedState = do
        let lat = SGR.staticReadLat $ derivedState ^. DS.slaveGroupRanges
            latestValues = SGR.staticReadAll lat (derivedState ^. DS.slaveGroupRanges)
        -- We only got here because the keySpaceRange didn't exist or didn't maybeExist, or because
        -- it existed. If it did exist, DSH would made a NewKeySpace out of it by now. Thus, to know
        -- how to processeed, we look for a NewKeySpace with the KeySpaceRange.
        if DSH.rangeMaybeExists keySpaceRange latestValues
          then
            -- The only reason we can be here is if we can go ahead and finish the deletion.
            NTM.performTask uid
              (derivedState ^. DS.networkTaskManager)
              (derivedState ^. DS.slaveGroupRanges)
              (derivedState ^. DS.slaveGroupEIds)
          else sendResponse CRsDD.DoesNotExist
      msgWrapper = Ms.MasterMessage . MM.MultiPaxosMessage
  in Ta.Task description tryHandling done msgWrapper

createPickKeySpace
  :: CRs.ClientResponse
  -> Co.EndpointId
  -> Co.SlaveGroupId
  -> Co.Timestamp
  -> Co.Choice
  -> Co.UID
  -> String
  -> Ta.Task DS.DerivedState MAc.OutputAction
createPickKeySpace response eId slaveGroupId timestamp choice uid description =
  let entry = PM.Master $ PM.PickKeySpace slaveGroupId choice uid
      tryHandling derivedState = do
        case derivedState ^. DS.slaveGroupRanges & SGR.staticRead slaveGroupId timestamp of
          Just (_, SGR.Changing _) -> return $ Left entry
          _ -> return $ Right ()
      done derivedState = do
        trace $ TrM.ClientResponseSent response
        addA $ MAc.Send [eId] $ Ms.ClientResponse response
        return ()
      msgWrapper = Ms.MasterMessage . MM.MultiPaxosMessage
  in Ta.Task description tryHandling done msgWrapper
