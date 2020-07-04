module Slave.SlaveInputHandler where

import qualified Data.List as Li
import qualified System.Random as Rn

import qualified Paxos.MultiPaxosInstance as MP
import qualified Paxos.PaxosLog as PL
import qualified Paxos.Tasks.PaxosTaskManager as PTM
import qualified Paxos.Tasks.Task as Ta
import qualified Proto.Actions.Actions as Ac
import qualified Proto.Common as Co
import qualified Proto.Messages as Ms
import qualified Proto.Messages.ClientRequests as CRq
import qualified Proto.Messages.ClientResponses as CRs
import qualified Proto.Messages.PaxosMessages as PM
import qualified Proto.Messages.SlaveMessages as SM
import qualified Proto.Messages.TraceMessages as TrM
import qualified Slave.DerivedState as DS
import qualified Slave.Env as En
import qualified Slave.SlaveState as SS
import qualified Slave.Internal_DerivedState as IDS
import qualified Slave.Internal_KeySpaceManager as IKSM
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

dbTableDNE = "The (database, table) doesn't exist."

handleInputAction
  :: Ac.InputAction
  -> ST SS.SlaveState ()
handleInputAction iAction =
  case iAction of
    Ac.Receive eId msg ->
      case msg of
        Ms.ClientRequest request -> do
          trace $ TrM.ClientRequestReceived request
          case request ^. CRq.payload of
            CRq.CreateDatabase databaseId tableId -> do
              let description = show (eId, request)
                  requestId = (request ^. CRq.meta.CRq.requestId)
                  task = createCreateDBTask eId description requestId (databaseId, tableId)
              handlingState .^ (PTM.handleTask task)
            CRq.ReadRequest databaseId tableId _ _ ->
              handleForwarding eId (databaseId, tableId) request
            CRq.WriteRequest databaseId tableId _ _ _ ->
              handleForwarding eId (databaseId, tableId) request
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
        Ms.TabletMessage keySpaceRange _ -> do
          ranges <- getL $ SS.derivedState.IDS.keySpaceManager.IKSM.ranges
          if Li.elem keySpaceRange ranges
            then addA $ Ac.Slave_Forward keySpaceRange eId msg
            else return ()
    Ac.RetryInput counterValue ->
      handlingState .^ PTM.handleRetry counterValue

handleForwarding
  :: Co.EndpointId
  -> (String, String)
  -> CRq.ClientRequest
  -> ST SS.SlaveState ()
handleForwarding eId (databaseId, tableId) request = do
  let range = Co.KeySpaceRange databaseId tableId Nothing Nothing
  ranges <- getL $ SS.derivedState.IDS.keySpaceManager.IKSM.ranges
  if Li.elem range ranges
    then addA $ Ac.Slave_Forward range eId $ Ms.ClientRequest request
    -- TODO we should be forwarding the request to the DM, since this Slave might just be behind.
    else addA $ Ac.Send [eId] $
          Ms.ClientResponse
            (CRs.ClientResponse
              (CRs.ResponseMeta (request ^. CRq.meta.CRq.requestId))
              (CRs.Error dbTableDNE))

createCreateDBTask
  :: Co.EndpointId
  -> String
  -> String
  -> (String, String)
  -> Ta.Task DS.DerivedState
createCreateDBTask eId description requestId (databaseId, tableId) =
  let tryHandling _ = do return False
      done _ = do
        let response =
              CRs.ClientResponse
                (CRs.ResponseMeta requestId)
                (CRs.Success)
        trace $ TrM.ClientResponseSent response
        addA $ Ac.Send [eId] $ Ms.ClientResponse response
      createPLEntry derivedState =
        let range = Co.KeySpaceRange databaseId tableId Nothing Nothing
            generation = (derivedState ^. IDS.keySpaceManager.IKSM.generation) + 1
        in PM.Slave $ PM.AddRange range generation
      msgWrapper = Ms.SlaveMessage . SM.MultiPaxosMessage
  in Ta.Task description tryHandling done createPLEntry msgWrapper
