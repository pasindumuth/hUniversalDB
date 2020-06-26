module ClientRequestManager where

import qualified Data.Maybe as Mb

import qualified PaxosLog as PL
import qualified Data.Sequence as Sq
import qualified MultiPaxosInstance as MP
import qualified MultiVersionKVStore as MS
import qualified Utils as U
import qualified Records.Actions.Actions as A
import qualified Records.Common.Common as C
import qualified Records.ClientRequestManager as CRM
import qualified Records.DerivedState as DS
import qualified Records.Env as E
import qualified Records.GlobalState as GS
import qualified Records.Messages.ClientMessages as CM
import qualified Records.Messages.Messages as M
import qualified Records.Messages.PaxosMessages as PM
import Lens (Lens', _1, _2, _3, _4, _5, (&), (^.), lp3, lp5, at)
import State

type HandlingState = (PL.PaxosLog, MP.MultiPaxosInstance, DS.DerivedState, CRM.ClientRequestManager, E.Env)

handlingState :: Lens' GS.GlobalState HandlingState
handlingState = (lp5 (GS.paxosLog, GS.multiPaxosInstance, GS.derivedState, GS.clientRequestManager, GS.env))

handleClientRequest :: C.EndpointId -> CM.ClientRequest -> ST HandlingState ()
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

handleRequest :: C.EndpointId -> CM.ClientRequest -> Int -> ST HandlingState ()
handleRequest eId request retryCount = do
  index <- _1 .^^^ PL.nextAvailableIndex
  let entry = createPLEntry request
  counterValue <- getL $ _4 . CRM.counter
  _4 . CRM.currentInsert .^^. \_ -> Just $ CRM.CurrentInsert index entry retryCount request eId counterValue
  lp3 (_2, _1, _5) .^ MP.handleMultiPaxos eId (PM.Insert entry)
  addA $ A.RetryOutput counterValue

handleRetry :: Int -> ST HandlingState ()
handleRetry counterValue = do
  currentInsertM <- getL $ _4 . CRM.currentInsert
  case currentInsertM of
    Just currentInsert | currentInsert ^. CRM.counterValue == counterValue -> do
      if currentInsert ^. CRM.retryCount == 2
        then do
          _4 .^ incrementCounter
          addA $ A.Send [currentInsert ^. CRM.eId] $ M.ClientResponse $ CM.Error "Timeout"
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
                  addA $ A.Send [eId] $ M.ClientResponse $ CM.ReadResponse value
                CM.WriteRequest _ _ _ -> do
                  addA $ A.Send [eId] $ M.ClientResponse $ CM.WriteResponse
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
                      addA $ A.Send [eId] $ M.ClientResponse $ CM.Error "Timeout"
                    else do
                      _4 .^ incrementCounter
                      handleRequest eId clientMessage (retryCount + 1)
        else return ()
    _ -> return ()

-- TODO: pass the data in unmodifiably.
tryHandling :: C.EndpointId -> CM.ClientRequest -> ST HandlingState Bool
tryHandling eId request = do
  kvstore <- getL $ _3 . DS.kvStore
  case request of
    CM.ReadRequest key timestamp -> do
      case MS.staticReadLat key kvstore of
        Just lat | timestamp <= lat -> do
          addA $ A.Send [eId] $ M.ClientResponse $ CM.ReadResponse $ MS.staticRead key timestamp kvstore
          return True
        _ -> return False
    CM.WriteRequest key _ timestamp ->
      case MS.staticReadLat key kvstore of
        Just lat | timestamp <= lat -> do
          addA $ A.Send [eId] $ M.ClientResponse $ CM.Error "Attempting to write into the past."
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
