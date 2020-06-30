module Slave.Tablet.TabletRequestManager where

import qualified Data.Maybe as Mb
import qualified Data.Sequence as Sq

import qualified Infra.Utils as U
import qualified Paxos.MultiPaxosInstance as MP
import qualified Paxos.PaxosLog as PL
import qualified Proto.Actions.Actions as Ac
import qualified Proto.Common as Co
import qualified Proto.Messages as Ms
import qualified Proto.Messages.ClientMessages as CM
import qualified Proto.Messages.PaxosMessages as PM
import qualified Slave.Tablet.MultiVersionKVStore as MS
import qualified Slave.Tablet.Internal.TabletRequestManager as TRM
import qualified Slave.Tablet.Internal.DerivedState as DS
import qualified Slave.Tablet.Internal.Env as En
import qualified Slave.Tablet.Internal.GlobalState as GS
import Infra.Lens
import Infra.State

type HandlingState = (PL.PaxosLog, MP.MultiPaxosInstance, DS.DerivedState, TRM.TabletRequestManager, En.Env)

handlingState :: Lens' GS.GlobalState HandlingState
handlingState = (lp5 (GS.paxosLog, GS.multiPaxosInstance, GS.derivedState, GS.tabletRequestManager, GS.env))

handleClientRequest :: Co.EndpointId -> CM.ClientRequest -> ST HandlingState ()
handleClientRequest eId request = do
  requestHandled <- tryHandling eId request
  if requestHandled
    then return ()
    else do
      requestQueue <- _4.TRM.requestQueue .^^. U.push (eId, request)
      if Sq.length requestQueue == 1
        then handleNextRequest
        else return ()

handleNextRequest :: ST HandlingState ()
handleNextRequest = do
  requestQueue <- getL $ _4.TRM.requestQueue
  if Sq.length requestQueue > 0
    then do
      let (eId, request) = U.peek requestQueue
      handleRequest eId request
    else return ()

pollAndNext :: ST HandlingState ()
pollAndNext = do
  _4 . TRM.requestQueue .^^ U.poll
  handleNextRequest

handleRequest :: Co.EndpointId -> CM.ClientRequest -> ST HandlingState ()
handleRequest eId request = do
  _4.TRM.currentInsert .^^. \_ -> Nothing
  requestHandled <- tryHandling eId request
  if requestHandled
    then pollAndNext
    else do
      index <- _1 .^^^ PL.nextAvailableIndex
      let entry = createPLEntry request
      _4.TRM.currentInsert .^^. \_ -> Just $ TRM.CurrentInsert index entry request eId
      slaveEIds <- getL $ _5.En.slaveEIds
      lp3 (_2, _1, _5.En.rand) .^ MP.handleMultiPaxos eId slaveEIds (PM.Insert entry)
      counterValue <- _4.TRM.counter .^^. (+1)
      addA $ Ac.RetryOutput counterValue

handleRetry :: Int -> ST HandlingState ()
handleRetry counterValue = do
  currentInsertM <- getL $ _4.TRM.currentInsert
  counter <- getL $ _4.TRM.counter
  case currentInsertM of
    Just (TRM.CurrentInsert _ _ clientMessage eId) | counter == counterValue -> do
      handleRequest eId clientMessage
    _ -> return ()

handleInsert :: ST HandlingState ()
handleInsert = do
  currentInsertM <- getL $ _4.TRM.currentInsert
  case currentInsertM of
    Just (TRM.CurrentInsert index entry clientMessage eId) -> do
      nextEntryM <- _1 .^^^ PL.getPLEntry index
      case nextEntryM of
        Just nextEntry -> do
          if nextEntry == entry
            then do
              case clientMessage of
                CM.ReadRequest key timestamp -> do
                  value <- _3 . DS.kvStore .^^^ MS.staticRead key timestamp
                  addA $ Ac.Send [eId] $ Ms.ClientResponse $ CM.ReadResponse value
                CM.WriteRequest _ _ _ -> do
                  addA $ Ac.Send [eId] $ Ms.ClientResponse $ CM.WriteResponse
              pollAndNext
            else handleRequest eId clientMessage
        _ -> return ()
    _ -> return ()

-- TODO: pass the data in unmodifiably.
-- Maybe this needs to be a const callback.
tryHandling :: Co.EndpointId -> CM.ClientRequest -> ST HandlingState Bool
tryHandling eId request = do
  kvstore <- getL $ _3 . DS.kvStore
  case request of
    CM.ReadRequest key timestamp -> do
      case MS.staticReadLat key kvstore of
        Just lat | timestamp <= lat -> do
          addA $ Ac.Send [eId] $ Ms.ClientResponse $ CM.ReadResponse $ MS.staticRead key timestamp kvstore
          return True
        _ -> return False
    CM.WriteRequest key _ timestamp ->
      case MS.staticReadLat key kvstore of
        Just lat | timestamp <= lat -> do
          addA $ Ac.Send [eId] $ Ms.ClientResponse $ CM.Error "Attempting to write into the past."
          return True
        _ -> return False

createPLEntry :: CM.ClientRequest -> PM.PaxosLogEntry
createPLEntry request =
  case request of
    CM.ReadRequest key timestamp -> PM.Read key timestamp
    CM.WriteRequest key value timestamp -> PM.Write key value timestamp
