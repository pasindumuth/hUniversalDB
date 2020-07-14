module Master.MasterInputHandler where

import qualified Data.Map as Mp
import qualified Data.Maybe as Mb
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
import qualified Proto.Messages.ClientResponses.CreateDatabase as CRsCD
import qualified Proto.Messages.PaxosMessages as PM
import qualified Proto.Messages.SlaveMessages as SM
import qualified Proto.Messages.TraceMessages as TrM
import qualified Master.DerivedState as DS
import qualified Master.Env as En
import qualified Master.MasterState as MS
import qualified Master.SlaveGroupRanges as SGR
import Infra.Lens
import Infra.State

handlingState :: Lens' MS.MasterState (
  MP.MultiPaxosInstance,
  DS.DerivedState,
  PTM.PaxosTaskManager DS.DerivedState,
  Rn.StdGen,
  [Co.EndpointId])
handlingState =
  (lp5 (
    MS.multiPaxosInstance,
    MS.derivedState,
    MS.paxosTaskManager,
    MS.env.En.rand,
    MS.env.En.slaveEIds))

handleInputAction
  :: Ac.InputAction
  -> ST MS.MasterState ()
handleInputAction iAction =
  case iAction of
    Ac.Receive eId msg ->
      case msg of
        Ms.ClientRequest request -> do
          let requestId = request ^. CRq.meta .CRq.requestId
          trace $ TrM.ClientRequestReceived request
          case request ^. CRq.payload of
            CRq.CreateDatabase databaseId tableId timestamp -> do
              let description = show (eId, request)
                  task = createDatabaseTask eId requestId databaseId tableId timestamp description
              handlingState .^ (PTM.handleTask task)
            _ -> U.caseError
        Ms.SlaveMessage slaveMsg ->
          case slaveMsg of
            SM.MultiPaxosMessage multiPaxosMsg -> do
              pl <- getL $ MS.multiPaxosInstance.MP.paxosLog
              slaveEIds <- getL $ MS.env.En.slaveEIds
              lp2 (MS.multiPaxosInstance, MS.env.En.rand)
                .^ MP.handleMultiPaxos eId slaveEIds multiPaxosMsg (Ms.SlaveMessage . SM.MultiPaxosMessage)
              pl' <- getL $ MS.multiPaxosInstance.MP.paxosLog
              if (pl /= pl')
                then do
                  addA $ Ac.Print $ ppShow pl'
                  paxosId <- getL $ MS.multiPaxosInstance.MP.paxosId
                  MS.derivedState .^ DS.handleDerivedState paxosId pl pl'
                  handlingState .^ PTM.handleInsert
                else return ()
        _ -> U.caseError
    Ac.RetryInput counterValue ->
      handlingState .^ PTM.handleRetry counterValue

createDatabaseTask
  :: Co.EndpointId
  -> Co.RequestId
  -> Co.DatabaseId
  -> Co.TableId
  -> Co.Timestamp
  -> String
  -> Ta.Task DS.DerivedState
createDatabaseTask eId requestId databaseId tableId timestamp description =
  let keySpaceRange = Co.KeySpaceRange databaseId tableId
      sendResponse responseValue = do
        let response =
              CRs.ClientResponse
                (CRs.Meta requestId)
                (CRs.CreateDatabase responseValue)
        trace $ TrM.ClientResponseSent response
        addA $ Ac.Send [eId] $ Ms.ClientResponse response
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
                              Just (_, SGR.New newKeySpace) ->
                                elem keySpaceRange (newKeySpace ^. Co.oldKeySpace) ||
                                elem keySpaceRange (newKeySpace ^. Co.newKeySpace)
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
                -- TODO: do this
                return ()
              Nothing -> U.caseError
      createPLEntry derivedState =
        PM.Master $ PM.CreateDatabase requestId databaseId tableId timestamp
      msgWrapper = Ms.SlaveMessage . SM.MultiPaxosMessage
  in Ta.Task description tryHandling done createPLEntry msgWrapper
