module Slave.ClientRequestManager where

import qualified Data.Maybe as Mb

import qualified Paxos.PaxosLog as PL
import qualified Data.Sequence as Sq
import qualified Paxos.MultiPaxosInstance as MP
import qualified Slave.MultiVersionKVStore as MS
import qualified Infra.Utils as U
import qualified Proto.Actions.Actions as Ac
import qualified Proto.Common as Co
import qualified Slave.Internal.ClientRequestManager as CRM
import qualified Slave.Internal.DerivedState as DS
import qualified Slave.Internal.Env as En
import qualified Slave.Internal.GlobalState as GS
import qualified Proto.Messages.ClientMessages as CM
import qualified Proto.Messages as Ms
import qualified Proto.Messages.PaxosMessages as PM
import Infra.Lens
import Infra.State

type HandlingState = (PL.PaxosLog, MP.MultiPaxosInstance, DS.DerivedState, CRM.ClientRequestManager, En.Env)

handlingState :: Lens' GS.GlobalState HandlingState
handlingState = (lp5 (GS.paxosLog, GS.multiPaxosInstance, GS.derivedState, GS.clientRequestManager, GS.env))

handleClientRequest :: Co.EndpointId -> CM.ClientRequest -> ST HandlingState ()
handleClientRequest eId request = do
  requestQueue <- _4 . CRM.requestQueue .^^. U.push (eId, request)
  if Sq.length requestQueue == 1
    then handleNextRequest
    else return ()

handleNextRequest :: ST HandlingState ()
handleNextRequest = do
  requestQueue <- getL $ _4 . CRM.requestQueue
  if Sq.length requestQueue > 0
    then do
      let (eId, request) = U.peek requestQueue
      requestHandled <- tryHandling eId request
      if requestHandled
        then pollAndNext
        else handleRequest eId request 0
    else return ()

pollAndNext :: ST HandlingState ()
pollAndNext = do
  _4 . CRM.requestQueue .^^ U.poll
  handleNextRequest

handleRequest :: Co.EndpointId -> CM.ClientRequest -> Int -> ST HandlingState ()
handleRequest eId request retryCount = do
  index <- _1 .^^^ PL.nextAvailableIndex
  let entry = createPLEntry request
  counterValue <- getL $ _4 . CRM.counter
  _4 . CRM.currentInsert .^^. \_ -> Just $ CRM.CurrentInsert index entry retryCount request eId counterValue
  lp3 (_2, _1, _5) .^ MP.handleMultiPaxos eId (PM.Insert entry)
  addA $ Ac.RetryOutput counterValue

handleRetry :: Int -> ST HandlingState ()
handleRetry counterValue = do
  currentInsertM <- getL $ _4 . CRM.currentInsert
  case currentInsertM of
    Just currentInsert | currentInsert ^. CRM.counterValue == counterValue -> do
      if currentInsert ^. CRM.retryCount == 2
        then do
          _4 .^ incrementCounter
          addA $ Ac.Send [currentInsert ^. CRM.eId] $ Ms.ClientResponse $ CM.Error "Timeout"
        else do
          _4 .^ incrementCounter
          handleRequest (currentInsert ^. CRM.eId) (currentInsert ^. CRM.clientMessage) (currentInsert ^. CRM.retryCount + 1)
    _ -> return ()

handleInsert :: ST HandlingState ()
handleInsert = do
  currentInsertM <- getL $ _4 . CRM.currentInsert
  currentCounter <- getL $ _4 . CRM.counter
  case currentInsertM of
    Just currentInsert | currentInsert ^. CRM.counterValue == currentCounter -> do
      let CRM.CurrentInsert index entry retryCount clientMessage eId counterValue = currentInsert
      nextAvailableIndex <- _1 .^^^ PL.nextAvailableIndex
      if nextAvailableIndex > index
        then do
          nextEntryM <- _1 .^^^ PL.getPLEntry index
          let Just nextEntry = nextEntryM
          if nextEntry == entry
            then do
              case clientMessage of
                CM.ReadRequest key timestamp -> do
                  value <- _3 . DS.kvStore .^^^ MS.staticRead key timestamp
                  addA $ Ac.Send [eId] $ Ms.ClientResponse $ CM.ReadResponse value
                CM.WriteRequest _ _ _ -> do
                  addA $ Ac.Send [eId] $ Ms.ClientResponse $ CM.WriteResponse
              _4 .^ incrementCounter
              pollAndNext
            else do
              requestHandled <- tryHandling eId clientMessage
              if requestHandled
                then do
                  _4 .^ incrementCounter
                  pollAndNext
                else
                  if retryCount == 2
                    then do
                      _4 .^ incrementCounter
                      addA $ Ac.Send [eId] $ Ms.ClientResponse $ CM.Error "Timeout"
                    else do
                      _4 .^ incrementCounter
                      handleRequest eId clientMessage (retryCount + 1)
        else return ()
    _ -> return ()

-- TODO: pass the data in unmodifiably.
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

incrementCounter :: ST CRM.ClientRequestManager ()
incrementCounter = do
  CRM.currentInsert .^^. \_ -> Nothing
  CRM.counter .^^. (+1)
  return ()
