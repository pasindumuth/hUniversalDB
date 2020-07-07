{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Debug.Trace as DTr
import qualified Control.Monad as Mo
import qualified Data.Map as Mp
import qualified Data.Set as St
import qualified System.Random as Rn
import Text.Show.Pretty (ppShow)

import qualified Infra.Utils as U
import qualified Proto.Common as Co
import qualified Proto.Messages as Ms
import qualified Proto.Messages.ClientRequests as CRq
import qualified Proto.Messages.ClientResponses as CRs
import qualified Proto.Messages.PaxosMessages as PM
import qualified Proto.Messages.TraceMessages as TrM
import qualified Slave.SlaveInputHandler as SIH
import qualified Slave.Tablet.Internal_MultiVersionKVStore as IMS
import qualified Slave.Tablet.MultiVersionKVStore as MS
import qualified Slave.Tablet.TabletInputHandler as TIH
import qualified TestState as Tt
import qualified SimulationManager as SM
import Infra.Lens
import Infra.State


-- We pass in a number of requests to send. There is a for loop.
-- On each iteration, we randomly selects the type of request to send.
--
-- 1. We keep track of the (database, table) we've issued CreateDatabase
--    requests to so far. This way, we can make sure to always issue non-duplicate
--    CreateDatabase requests.
--
-- 2. We keep track of keys that have been written to. This is useful for selecting
--    when to try overwriting a key and when to try writing a new key. Similarly,
--    for selecting when to try reading an existing key or a non-existant key.
--
-- 3. On every request, we select the timestamp approximately equal to the number
--    of requests that were already sent. However, we add a slight bit of noise to this
--    to simulate sending requests to the past (which sometimes we expect the system
--    to reply with an error).
generateRequest :: ST Tt.TestState ()
generateRequest = do
  numTables <- Tt.numTableKeys .^^^ Mp.size
  slaveId :: Int <- Tt.rand .^^ Rn.randomR (0, 4)
  requestType :: Int <- Tt.rand .^^ Rn.randomR (0, 2)
  case requestType of
    0 -> do
      let tableId = "t" ++ show numTables
      SM.addClientMsg slaveId (CRq.CreateDatabase "d" tableId)
      Tt.requestStats.Tt.numCreateDBRqs .^^. (+1)
      Tt.numTableKeys .^^. Mp.insert numTables 0
      return ()
    _ | requestType == 1 || requestType == 2 -> do
      newTableProb :: Int <- Tt.rand .^^ Rn.randomR (0, 99)
      (tableIdx, keyIdx) <-
        if newTableProb >= 75 || numTables == 0
          then return (numTables, 0)
          else do
            tableIdx :: Int <- Tt.rand .^^ Rn.randomR (0, numTables - 1)
            keyIdx <- do
              res <- Tt.numTableKeys .^^^ Mp.lookup tableIdx
              case res of
                Just numKeys | numKeys > 0 -> do
                  newKeyProb :: Int <- Tt.rand .^^ Rn.randomR (0, 99)
                  if newKeyProb >= 75
                    then do
                      Tt.numTableKeys .^^. Mp.insert tableIdx (numKeys + 1)
                      return numKeys
                    else do
                      keyIdx :: Int <- Tt.rand .^^ Rn.randomR (0, numKeys - 1)
                      return keyIdx
                _ -> do
                  Tt.numTableKeys .^^. Mp.insert tableIdx 1
                  return 0
            return (tableIdx, keyIdx)
      let tableId = "t" ++ show tableIdx
          key = "k" ++ show keyIdx
      noise <- Tt.rand .^^ Rn.randomR (-2, 2)
      trueTimestamp <- getL $ Tt.trueTimestamp
      let timestamp = trueTimestamp + noise
      case requestType of
        1 -> do
          let value = "v" ++ show keyIdx
          SM.addClientMsg slaveId (CRq.Write "d" tableId key value timestamp)
          Tt.requestStats.Tt.numWriteRqs .^^. (+1)
        2 -> do
          SM.addClientMsg slaveId (CRq.Read "d" tableId key timestamp)
          Tt.requestStats.Tt.numReadRqs .^^. (+1)
      return ()

analyzeResponses :: ST Tt.TestState ()
analyzeResponses = do
  clientResponses <- Tt.clientResponses .^^^ Mp.toList
  Mo.forM_ clientResponses $ \(_, responses) ->
    Mo.forM_ responses $ \(Ms.ClientResponse response) -> do
      case response ^. CRs.responsePayload of
        CRs.ReadResponse (CRs.ReadSuccess _) -> Tt.requestStats.Tt.numReadSuccessRss .^^. (+1)
        CRs.ReadResponse CRs.ReadUnknownDB -> Tt.requestStats.Tt.numReadUnknownDBRss .^^. (+1)
        CRs.WriteResponse CRs.WriteSuccess -> Tt.requestStats.Tt.numWriteSuccessRss .^^. (+1)
        CRs.WriteResponse CRs.WriteUnknownDB -> Tt.requestStats.Tt.numWriteUnknownDBRss .^^. (+1)
        CRs.WriteResponse CRs.BackwardsWrite -> Tt.requestStats.Tt.numBackwardsWriteRss .^^. (+1)
        CRs.CreateDBResponse CRs.CreateDBSuccess -> Tt.requestStats.Tt.numCreateDBSuccessRss .^^. (+1)

test1 :: ST Tt.TestState ()
test1 = do
  SM.addClientMsg 0 (CRq.CreateDatabase "d" "t"); SM.simulateAll
  SM.addClientMsg 1 (CRq.Write "d" "t" "key1" "value1" 1); SM.simulateAll
  SM.addClientMsg 2 (CRq.Read "d" "t" "key1" 3); SM.simulateAll
  SM.addClientMsg 3 (CRq.Write "d" "t" "key1" "value1" 2); SM.simulateAll
  SM.addClientMsg 2 (CRq.Read "d" "t" "key2" 3); SM.simulateAll

test2 :: ST Tt.TestState ()
test2 = do
  SM.addClientMsg 0 (CRq.CreateDatabase "d" "t"); SM.simulateN 2
  SM.addClientMsg 1 (CRq.Write "d" "t" "key1" "value1" 1); SM.simulateN 2
  SM.addClientMsg 2 (CRq.Write "d" "t" "key2" "value2" 2); SM.simulateN 2
  SM.addClientMsg 3 (CRq.Write "d" "t" "key3" "value3" 3); SM.simulateN 2
  SM.addClientMsg 4 (CRq.Write "d" "t" "key4" "value4" 4); SM.simulateN 2
  SM.addClientMsg 0 (CRq.Write "d" "t" "key5" "value5" 5); SM.simulateAll

-- TODO: I want more monitoring of what happens during a test. Maybe
-- I need message-drop stats.
test3 :: ST Tt.TestState ()
test3 = do
  Mo.forM_ [1..50] $
    \_ -> do
      generateRequest
      SM.simulateN 2
      SM.dropMessages 1
  SM.simulateAll
  analyzeResponses

test4 :: ST Tt.TestState ()
test4 = do
  Mo.forM_ [1..20] $
    \_ -> do
      Mo.forM_ [1..5] $
        \_ -> generateRequest
      SM.simulateN 2
      SM.dropMessages 2
  SM.simulateAll
  analyzeResponses

data TestPaxosLog = TestPaxosLog {
  _plog :: Mp.Map PM.IndexT PM.PaxosLogEntry,
  _nextIdx :: PM.IndexT
}

makeLenses ''TestPaxosLog

type TestPaxosLogs = Mp.Map Co.PaxosId TestPaxosLog

addMsgs
 :: Co.PaxosId
 -> PM.IndexT
 -> Mp.Map PM.IndexT PM.PaxosLogEntry
 -> [TrM.TraceMessage]
 -> (PM.IndexT, [TrM.TraceMessage])
addMsgs paxosId i m msgs =
  case Mp.lookup i m of
    Just v -> addMsgs paxosId (i + 1) m (TrM.PaxosInsertion paxosId i v : msgs)
    Nothing -> (i, msgs)

-- This functions restructures the messages so that PaxosInsertions occur in
-- order of their index, where the occur as early as they are first seen. This function
-- also checks if the PaxosLogInsertion are consistent (i.e. the entry for a PaxosLogInsertion
-- at a given PaxosId and Index are the same).
refineTrace :: [TrM.TraceMessage] -> Either Co.ErrorMsg [TrM.TraceMessage]
refineTrace msgs =
    let paxosLogsE = U.s31 Mo.foldM (Mp.empty, []) msgs $
          \(paxosLogs, modMsgs) msg ->
            case msg of
              TrM.PaxosInsertion paxosId index entry ->
                case paxosLogs ^. at paxosId of
                  Just paxosLog ->
                    case paxosLog ^. plog . at index of
                      Just entry' ->
                        if entry' == entry
                          then Right (paxosLogs, modMsgs)
                          else Left $ "A PaxosLog entry mismatch occurred at: " ++
                                      "PaxosId = " ++ show paxosId ++ ", Entry: " ++ show entry
                      _ ->
                        let plog' = paxosLog ^. plog & at index ?~ entry
                            (nextIdx', modMsgs') = addMsgs paxosId (paxosLog ^. nextIdx) plog' modMsgs
                            paxosLog' = TestPaxosLog plog' nextIdx'
                            paxosLogs' = paxosLogs & at paxosId ?~ paxosLog'
                        in Right (paxosLogs', modMsgs')
                  Nothing ->
                    let plog' = Mp.empty & at index ?~ entry
                        (nextIdx', modMsgs') = addMsgs paxosId 0 plog' modMsgs
                        paxosLog' = TestPaxosLog plog' nextIdx'
                        paxosLogs' = paxosLogs & at paxosId ?~ paxosLog'
                    in Right (paxosLogs', modMsgs')
              _ -> Right (paxosLogs, (msg:modMsgs))
    in case paxosLogsE of
      Right (_, modMsgs) -> Right $ reverse modMsgs
      Left errMsg -> Left errMsg

type RequestMap = Mp.Map Co.RequestId CRq.Payload
-- The range of `RangeMap` should always be the domain of `Tables`
type RangeMap = Mp.Map Co.PaxosId (Co.DatabaseId, Co.TableId)
type Tables = Mp.Map (Co.DatabaseId, Co.TableId) IMS.MultiVersionKVStore

-- Notice that we often use fatally failing operations, like ^?!. This is because
-- if the program is working right, then these operations shouldn't fail.
-- Rather than checking whether the oepration would fail or not by using
-- safe operations, it's more compact to just fail fatally.
checkResponses :: [TrM.TraceMessage] -> Either String ()
checkResponses msgs =
  let genericError response payload = Left $ "Response " ++ show response ++ " is the wrong response to Request " ++ show payload
      plEntryError entry = Left $ "Unwarranted PaxosLogEntry: " ++ show entry
      testRes = U.s31 Mo.foldM (Mp.empty, Mp.empty, Mp.empty) msgs $
        \(requestMap, rangeMap, tables) msg ->
          case msg of
            TrM.PaxosInsertion paxosId _ entry ->
              case entry of
                PM.Tablet entry ->
                  let (databaseId, tableId) = rangeMap ^?! ix paxosId
                  in case entry of
                    PM.Read requestId key timestamp ->
                      case requestMap ^. at requestId of
                        Nothing -> plEntryError entry
                        Just payload ->
                          case payload of
                            CRq.Read databaseId' tableId' key' timestamp'
                              | databaseId' == databaseId &&
                                tableId' == tableId &&
                                key' == key &&
                                timestamp' == timestamp ->
                              let (_, tables') = tables %^^* (ix (databaseId, tableId)) $ MS.read key timestamp
                              in Right (requestMap, rangeMap, tables')
                            _ -> plEntryError entry
                    PM.Write requestId key value timestamp ->
                      case requestMap ^. at requestId of
                        Nothing -> plEntryError entry
                        Just payload ->
                          case payload of
                            CRq.Write databaseId' tableId' key' value' timestamp'
                              | databaseId' == databaseId &&
                                tableId' == tableId &&
                                key' == key &&
                                value' == value &&
                                timestamp' == timestamp ->
                              let (_, tables') = tables %^^* (ix (databaseId, tableId)) $ MS.write key value timestamp
                              in Right (requestMap, rangeMap, tables')
                            _ -> plEntryError entry
                PM.Slave entry ->
                  case entry of
                    PM.AddRange requestId range@(Co.KeySpaceRange databaseId tableId _ _) generation ->
                      case requestMap ^. at requestId of
                        Nothing -> plEntryError entry
                        Just payload ->
                          case payload of
                            CRq.CreateDatabase databaseId' tableId'
                              | databaseId' == databaseId &&
                                tableId' == tableId ->
                              let rangeMap' = rangeMap & at (show range) ?~ (databaseId, tableId)
                                  tables' = tables & at (databaseId, tableId) ?~ Mp.empty
                              in Right (requestMap, rangeMap', tables')
                            _ -> plEntryError entry
            TrM.ClientRequestReceived (CRq.ClientRequest (CRq.Meta requestId) payload) ->
              Right (requestMap & at requestId ?~ payload, rangeMap, tables)
            TrM.ClientResponseSent response@(CRs.ClientResponse (CRs.Meta requestId) responsePayload) ->
              case requestMap ^. at requestId of
                Nothing -> Left $ "Response " ++ show response ++ " has no corresponding request."
                Just payload -> do
                  let genericSuccess = Right (requestMap & Mp.delete requestId, rangeMap, tables)
                  case payload of
                    CRq.CreateDatabase databaseId tableId ->
                      case responsePayload of
                        CRs.CreateDBResponse CRs.CreateDBSuccess -> genericSuccess
                        _ -> genericError response payload
                    CRq.Read databaseId tableId key timestamp ->
                      case tables ^. at (databaseId, tableId) of
                        Just table ->
                          case responsePayload of
                            CRs.ReadResponse (CRs.ReadSuccess val)
                              | val == (MS.staticRead key timestamp table) -> genericSuccess
                            -- TODO: An Error is possible until requests get routed to the DM if the slave is behind
                            CRs.ReadResponse CRs.ReadUnknownDB -> genericSuccess
                            _ -> genericError response payload
                        Nothing ->
                          case responsePayload of
                            CRs.ReadResponse CRs.ReadUnknownDB -> genericSuccess
                            _ -> genericError response payload
                    CRq.Write databaseId tableId key value timestamp ->
                      case tables ^. at (databaseId, tableId) of
                        Just table ->
                          case MS.staticReadLat key table of
                            Just lat ->
                              case responsePayload of
                                -- The lat should be equal to timestamp. This is because by this point in
                                -- the trace messages, the write PaxosLogEntry should have been encoutered.
                                -- TODO: it's possible that the `lat` was already here when the Write was received
                                -- but that we're accidentally returning a `Write` instead of an error. We must
                                -- figure out how to handle this issue.
                                CRs.WriteResponse CRs.WriteSuccess 
                                  | lat == timestamp -> genericSuccess
                                CRs.WriteResponse CRs.BackwardsWrite -> genericSuccess
                                -- TODO: An Error is possible until requests get routed to the DM if the slave is behind
                                CRs.WriteResponse CRs.WriteUnknownDB -> genericSuccess
                                _ -> genericError response payload
                            Nothing -> do
                              -- TODO: this case should result in a genericError when the datamasters are set up
                              case responsePayload of
                                CRs.WriteResponse CRs.WriteUnknownDB -> genericSuccess
                                _ -> genericError response payload
                        Nothing ->
                          case responsePayload of
                            CRs.WriteResponse CRs.WriteUnknownDB -> genericSuccess
                            _ -> genericError response payload
  in case testRes of
    Right (requestMap, _, _) ->
      if Mp.size requestMap > 0
        then Left $ "The following requests went unanswered: " ++ show requestMap
        else Right ()
    Left errMsg -> Left errMsg

-- This list of trace messages includes all events across all slaves.
verifyTrace :: [TrM.TraceMessage] -> Either String ()
verifyTrace msgs =
  -- First, we restructure the messages so that PaxosInsertions happen in order of
  -- ascending index, ensuring that the entries are all consistent.
  let paxosLogsE = refineTrace msgs
  in case paxosLogsE of
    Left errMsg -> Left errMsg
    Right msgs ->
      -- Next, we construct the mvkvs and ensure that the responses made to each client
      -- request are correct (at the time the responses are issued).
      let responseCheck = checkResponses msgs
      in case responseCheck of
        Left errMsg -> Left errMsg
        Right _ -> Right ()

driveTest :: Int -> ST Tt.TestState () -> IO ()
driveTest testNum test = do
  let g = SM.createTestState 0 5 1
      (_, (oActions, traceMsgs, g')) = runST test g
      -- We must reverse traceMsgs since that's created in reverse order,
      -- with the most recent message first
      testMsg =
        case verifyTrace $ reverse traceMsgs of
          Left errMsg -> "Test " ++ (show testNum) ++ " Failed! Error: " ++ errMsg
          Right _ -> "Test " ++ (show testNum) ++ " Passed! " ++ (show $ length traceMsgs) ++ " trace messages."
  putStrLn $ ppShow $ g' ^. Tt.requestStats
  putStrLn testMsg
  putStrLn ""
  return ()

testDriver :: IO ()
testDriver = do
  driveTest 1 test1
  driveTest 2 test2
  driveTest 3 test3
  driveTest 4 test4

main :: IO ()
main = testDriver
