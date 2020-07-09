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
import qualified Proto.Messages.ClientResponses.RangeWrite as CRsRW
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
import qualified Slave.Internal_KeySpaceManager as IKSM
import Infra.Lens
import Infra.State

dbTableDNE = "The (database, table) doesn't exist."

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

getRanges :: ST SS.SlaveState [Co.KeySpaceRange]
getRanges = do
  versions <- getL $ SS.derivedState.IDS.keySpaceManager.IKSM.versions
  case versions of
    [] -> return []
    (version:_) -> return $ version ^. _3

handleInputAction
  :: Ac.InputAction
  -> ST SS.SlaveState ()
handleInputAction iAction =
  case iAction of
    Ac.Receive eId msg ->
      case msg of
        Ms.ClientRequest request -> do
          let requestId = (request ^. CRq.meta . CRq.requestId)
          trace $ TrM.ClientRequestReceived request
          case request ^. CRq.payload of
            CRq.RangeWrite databaseId tableId timestamp -> do
              ranges <- getRanges
              let description = show (eId, request)
                  task = createCreateDBTask eId requestId (databaseId, tableId) timestamp ranges description
              handlingState .^ (PTM.handleTask task)
            CRq.SlaveRead databaseId tableId key timestamp -> do
              let range = Co.KeySpaceRange databaseId tableId
              ranges <- getRanges
              if Li.elem range ranges
                then addA $ Ac.TabletForward range eId $ TM.ForwardedClientRequest $
                       TM.ClientRequest
                         (TM.Meta requestId)
                         (TM.TabletRead key timestamp)
                else do
                -- TODO we should be forwarding the request to the DM, since this Slave might just be behind.
                  let response =
                        (CRs.ClientResponse
                          (CRs.Meta requestId)
                          (CRs.SlaveRead CRsSR.UnknownDB))
                  trace $ TrM.ClientResponseSent response
                  addA $ Ac.Send [eId] $ Ms.ClientResponse response
            CRq.SlaveWrite databaseId tableId key value timestamp -> do
              let range = Co.KeySpaceRange databaseId tableId
              ranges <- getRanges
              if Li.elem range ranges
                then addA $ Ac.TabletForward range eId $ TM.ForwardedClientRequest $
                       TM.ClientRequest
                         (TM.Meta requestId)
                         (TM.TabletWrite key value timestamp)
                else do
                -- TODO we should be forwarding the request to the DM, since this Slave might just be behind.
                  let response =
                        (CRs.ClientResponse
                          (CRs.Meta requestId)
                          (CRs.SlaveWrite CRsSW.UnknownDB))
                  trace $ TrM.ClientResponseSent response
                  addA $ Ac.Send [eId] $ Ms.ClientResponse response
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
          ranges <- getRanges
          if Li.elem keySpaceRange ranges
            then addA $ Ac.TabletForward keySpaceRange eId tabletMsg
            else return ()
    Ac.RetryInput counterValue ->
      handlingState .^ PTM.handleRetry counterValue

-- TODO: this needs to be completely redone, but we need a proper RangeWrite Client Request.
createCreateDBTask
  :: Co.EndpointId
  -> String
  -> (String, String)
  -> Co.Timestamp
  -> [Co.KeySpaceRange]
  -> String
  -> Ta.Task DS.DerivedState
createCreateDBTask eId requestId (databaseId, tableId) timestamp ranges  description =
  let tryHandling derivedState = do
        let lat = derivedState ^. IDS.keySpaceManager.IKSM.lat
        if lat < timestamp
          then return False
          else do
            let response =
                  (CRs.ClientResponse
                    (CRs.Meta requestId)
                    (CRs.RangeWrite CRsRW.BackwardsWrite))
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
      createPLEntry derivedState =
        let range = Co.KeySpaceRange databaseId tableId
        in PM.Slave $ PM.RangeWrite requestId timestamp (range:ranges)
      msgWrapper = Ms.SlaveMessage . SM.MultiPaxosMessage
  in Ta.Task description tryHandling done createPLEntry msgWrapper
