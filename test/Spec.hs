{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Monad as Mo
import qualified Data.Map as Mp
import qualified Data.Set as St
import qualified System.Random as Rn

import qualified Infra.Utils as U
import qualified Proto.Common as Co
import qualified Proto.Messages as Ms
import qualified Proto.Messages.ClientRequests as CRq
import qualified Proto.Messages.ClientResponses as CRs
import qualified Proto.Messages.ClientResponses.RangeRead as CRsRR
import qualified Proto.Messages.ClientResponses.RangeWrite as CRsRW
import qualified Proto.Messages.ClientResponses.SlaveRead as CRsSR
import qualified Proto.Messages.ClientResponses.SlaveWrite as CRsSW
import qualified Proto.Messages.TraceMessages as TrM
import qualified TestState as Tt
import qualified SimulationManager as SM
import qualified TraceChecker as TC
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
      -- TODO: this just simulates adding new KeySpaceRanges. Make it do more general things.
      let ranges = [0..numTables] & map (\i -> Co.KeySpaceRange "d" ("t" ++ show i))
      timestamp <- makeTimestamp
      SM.addClientMsg slaveId (CRq.RangeWrite ranges timestamp)
      Tt.requestStats.Tt.numRangeWriteRqs .^^. (+1)
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
      timestamp <- makeTimestamp
      case requestType of
        1 -> do
          let value = "v" ++ show keyIdx
          SM.addClientMsg slaveId (CRq.SlaveWrite "d" tableId key value timestamp)
          Tt.requestStats.Tt.numWriteRqs .^^. (+1)
        2 -> do
          SM.addClientMsg slaveId (CRq.SlaveRead "d" tableId key timestamp)
          Tt.requestStats.Tt.numReadRqs .^^. (+1)
        _ -> U.caseError
      return ()
    _ -> U.caseError
  where
    makeTimestamp = do
      noise <- Tt.rand .^^ Rn.randomR (-2, 2)
      trueTimestamp <- getL $ Tt.trueTimestamp
      return $ trueTimestamp + noise

analyzeResponses :: ST Tt.TestState ()
analyzeResponses = do
  clientResponses <- Tt.clientResponses .^^^ Mp.toList
  Mo.forM_ clientResponses $ \(_, responses) ->
    Mo.forM_ responses $ \(Ms.ClientResponse response) -> do
      case response ^. CRs.payload of
        CRs.SlaveRead (CRsSR.Success _) -> Tt.requestStats.Tt.numReadSuccessRss .^^. (+1)
        CRs.SlaveRead CRsSR.UnknownDB -> Tt.requestStats.Tt.numReadUnknownDBRss .^^. (+1)
        CRs.SlaveWrite CRsSW.Success -> Tt.requestStats.Tt.numWriteSuccessRss .^^. (+1)
        CRs.SlaveWrite CRsSW.UnknownDB -> Tt.requestStats.Tt.numWriteUnknownDBRss .^^. (+1)
        CRs.SlaveWrite CRsSW.BackwardsWrite -> Tt.requestStats.Tt.numBackwardsWriteRss .^^. (+1)
        CRs.RangeRead (CRsRR.Success _) -> Tt.requestStats.Tt.numRangeReadSuccessRss .^^. (+1)
        CRs.RangeWrite CRsRW.Success -> Tt.requestStats.Tt.numRangeWriteSuccessRss .^^. (+1)
        CRs.RangeWrite CRsRW.BackwardsWrite -> Tt.requestStats.Tt.numRangeWriteBackwardsWriteRss .^^. (+1)

test1 :: ST Tt.TestState ()
test1 = do
  SM.addClientMsg 0 (CRq.RangeWrite [Co.KeySpaceRange "d" "t"] 0); SM.simulateAll
  SM.addClientMsg 1 (CRq.SlaveWrite "d" "t" "key1" "value1" 1); SM.simulateAll
  SM.addClientMsg 2 (CRq.SlaveRead "d" "t" "key1" 3); SM.simulateAll
  SM.addClientMsg 3 (CRq.SlaveWrite "d" "t" "key1" "value1" 2); SM.simulateAll
  SM.addClientMsg 2 (CRq.SlaveRead "d" "t" "key2" 3); SM.simulateAll

test2 :: ST Tt.TestState ()
test2 = do
  SM.addClientMsg 0 (CRq.RangeWrite [Co.KeySpaceRange "d" "t"] 0); SM.simulateN 2
  SM.addClientMsg 1 (CRq.SlaveWrite "d" "t" "key1" "value1" 1); SM.simulateN 2
  SM.addClientMsg 2 (CRq.SlaveWrite "d" "t" "key2" "value2" 2); SM.simulateN 2
  SM.addClientMsg 3 (CRq.SlaveWrite "d" "t" "key3" "value3" 3); SM.simulateN 2
  SM.addClientMsg 4 (CRq.SlaveWrite "d" "t" "key4" "value4" 4); SM.simulateN 2
  SM.addClientMsg 0 (CRq.SlaveWrite "d" "t" "key5" "value5" 5); SM.simulateAll

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

-- This list of trace messages includes all events across all slaves.
verifyTrace :: [TrM.TraceMessage] -> Either String ()
verifyTrace msgs =
  -- First, we restructure the messages so that PaxosInsertions happen in order of
  -- ascending index, ensuring that the entries are all consistent.
  let paxosLogsE = TC.refineTrace msgs
  in case paxosLogsE of
    Left errMsg -> Left errMsg
    Right msgs ->
      -- Next, we construct the mvkvs and ensure that the responses made to each client
      -- request are correct (at the time the responses are issued).
      let responseCheck = TC.checkMsgs msgs
      in case responseCheck of
        Left errMsg -> Left errMsg
        Right _ -> Right ()

driveTest :: Int -> ST Tt.TestState () -> IO ()
driveTest testNum test = do
  let g = SM.createTestState 0 5 1
      (_, (_, traceMsgs, g')) = runST test g
      -- We must reverse traceMsgs since that's created in reverse order,
      -- with the most recent message first
      testMsg =
        case verifyTrace $ traceMsgs of
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
