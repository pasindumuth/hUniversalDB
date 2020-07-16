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
import qualified Proto.Messages.ClientResponses.CreateDatabase as CRsCD
import qualified Proto.Messages.ClientResponses.DeleteDatabase as CRsDD
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

data RequestType =
       CreateDatabase |
       RangeRead |
       RangeWrite |
       SlaveRead |
       SlaveWrite

type RequestTypeDist = Int -> RequestType

slaveDist :: RequestTypeDist
slaveDist r
  | r < 15 = RangeRead
  | r < 30 = RangeWrite
  | r < 65 = SlaveRead
  | otherwise = SlaveWrite

allRequestsDist :: RequestTypeDist
allRequestsDist r
  | r < 10 = CreateDatabase
  | r < 20 = RangeRead
  | r < 30 = RangeWrite
  | r < 65 = SlaveRead
  | otherwise = SlaveWrite

masterDist :: RequestTypeDist
masterDist r
  | r < 30 = CreateDatabase
  | r < 65 = SlaveRead
  | otherwise = SlaveWrite

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
genRequest :: RequestTypeDist -> STS Tt.ClientState (SM.Endpoint, CRq.Payload)
genRequest requestDist = do
  numTablets <- Tt.numTabletKeys .^^^ Mp.size
  eIdIdx :: Int <- Tt.clientRand .^^ Rn.randomR (0, 4)
  let pickRangeAndKey = do
        newTableProb :: Int <- Tt.clientRand .^^ Rn.randomR (0, 99)
        curRanges <- getL $ Tt.curRanges
        if newTableProb >= 75 || (St.size curRanges) == 0
          then return (makeRange numTablets, 0)
          else do
            tabletIdx :: Int <- Tt.clientRand .^^ Rn.randomR (0, (St.size curRanges) - 1)
            let range = St.elemAt tabletIdx curRanges
            keyIdx <- do
              res <- Tt.numTabletKeys .^^^ Mp.lookup range
              case res of
                Just numKeys | numKeys > 0 -> do
                  newKeyProb :: Int <- Tt.clientRand .^^ Rn.randomR (0, 99)
                  if newKeyProb >= 75
                    then do
                      Tt.numTabletKeys .^^. Mp.insert range (numKeys + 1)
                      return numKeys
                    else do
                      keyIdx :: Int <- Tt.clientRand .^^ Rn.randomR (0, numKeys - 1)
                      return keyIdx
                _ -> do
                  Tt.numTabletKeys .^^. Mp.insert range 1
                  return 0
            return (range, keyIdx)
  r <- Tt.clientRand .^^ Rn.randomR (0, 99)
  case requestDist r of
    CreateDatabase -> do
      curRanges <- getL $ Tt.curRanges
      addNewRangeProb :: Int <- Tt.clientRand .^^ Rn.randomR (0, 99)
      (Co.KeySpaceRange databaseId tableId, curRanges) <-
        if addNewRangeProb > 50 || (St.size curRanges) == 0
          then do
            let range = makeRange numTablets
            Tt.numTabletKeys .^^. Mp.insert range 0
            return (range, curRanges & St.insert range)
          else do
            tabletIdx :: Int <- Tt.clientRand .^^ Rn.randomR (0, (St.size curRanges) - 1)
            let range = St.elemAt tabletIdx curRanges
            return (range, curRanges)
      Tt.curRanges .^^. \_ -> curRanges
      timestamp <- makeTimestamp
      -- We make the Database in the future to avoid racing other requests.
      -- We might need a more sophisticated approach of locking the slaves before
      -- trying to CreateDatabase
      return (SM.Master eIdIdx, CRq.CreateDatabase databaseId tableId (timestamp + 20))
    RangeRead -> do
      timestamp <- makeTimestamp
      return (SM.Slave eIdIdx, CRq.RangeRead timestamp)
    RangeWrite -> do
      allRanges <- Tt.numTabletKeys .^^^ Mp.keys
      curRanges <- U.s31 Mo.foldM St.empty allRanges $ \curRanges range -> do
        addRange <- Tt.clientRand .^^ Rn.random
        if addRange
          then return $ curRanges & St.insert range
          else return $ curRanges
      addNewRange <- Tt.clientRand .^^ Rn.random
      curRanges <- if addNewRange
                      then do
                        let range = makeRange numTablets
                        Tt.numTabletKeys .^^. Mp.insert range 0
                        return $ curRanges & St.insert range
                      else return $ curRanges
      Tt.curRanges .^^. \_ -> curRanges
      timestamp <- makeTimestamp
      return (SM.Slave eIdIdx, CRq.RangeWrite (St.toList curRanges) timestamp)
    SlaveRead -> do
      (Co.KeySpaceRange databaseId tableId, keyIdx) <- pickRangeAndKey
      let key = "k" ++ show keyIdx
          value = "v" ++ show keyIdx
      timestamp <- makeTimestamp
      return (SM.Slave eIdIdx, CRq.SlaveWrite databaseId tableId key value timestamp)
    SlaveWrite -> do
      (Co.KeySpaceRange databaseId tableId, keyIdx) <- pickRangeAndKey
      let key = "k" ++ show keyIdx
      timestamp <- makeTimestamp
      return (SM.Slave eIdIdx, CRq.SlaveRead databaseId tableId key timestamp)
  where
    makeTimestamp = do
      noise <- Tt.clientRand .^^ Rn.randomR (-2, 2)
      trueTimestamp <- getL $ Tt.trueTimestamp
      return $ trueTimestamp + noise
    makeRange i = Co.KeySpaceRange "d" ("t" ++ show i)

analyzeResponses :: STS Tt.ClientState ()
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
        CRs.CreateDatabase CRsCD.BackwardsWrite -> Tt.requestStats.Tt.numCreateDatabaseBackwardsWriteRss .^^. (+1)
        CRs.CreateDatabase CRsCD.AlreadyExists -> Tt.requestStats.Tt.numCreateDatabaseAlreadyExistsRss .^^. (+1)
        CRs.CreateDatabase CRsCD.NothingChanged -> Tt.requestStats.Tt.numCreateDatabaseNothingChangedRss .^^. (+1)
        CRs.CreateDatabase CRsCD.Success -> Tt.requestStats.Tt.numCreateDatabaseSuccessRss .^^. (+1)
        CRs.DeleteDatabase CRsDD.BackwardsWrite -> Tt.requestStats.Tt.numDeleteDatabaseBackwardsWriteRss .^^. (+1)
        CRs.DeleteDatabase CRsDD.DoesNotExist -> Tt.requestStats.Tt.numDeleteDatabaseAlreadyExistsRss .^^. (+1)
        CRs.DeleteDatabase CRsDD.NothingChanged -> Tt.requestStats.Tt.numDeleteDatabaseNothingChangedRss .^^. (+1)
        CRs.DeleteDatabase CRsDD.Success -> Tt.requestStats.Tt.numDeleteDatabaseSuccessRss .^^. (+1)

test1 :: STS Tt.TestState ()
test1 = do
  SM.addClientMsg (SM.Slave 0) (CRq.RangeWrite [Co.KeySpaceRange "d" "t"] 1); SM.simulateN 2
  SM.addClientMsg (SM.Slave 1) (CRq.SlaveWrite "d" "t" "key1" "value1" 2); SM.simulateN 2
  SM.addClientMsg (SM.Slave 2) (CRq.SlaveWrite "d" "t" "key2" "value2" 3); SM.simulateN 2
  SM.addClientMsg (SM.Slave 3) (CRq.SlaveWrite "d" "t" "key3" "value3" 4); SM.simulateN 2
  SM.addClientMsg (SM.Slave 4) (CRq.SlaveWrite "d" "t" "key4" "value4" 5); SM.simulateN 2
  SM.addClientMsg (SM.Slave 0) (CRq.SlaveWrite "d" "t" "key5" "value5" 6); SM.simulateAll
  Tt.clientState .^ analyzeResponses

test2 :: STS Tt.TestState ()
test2 = do
  Mo.forM_ [1..100] $
    \_ -> do
      (endpoint, payload) <- Tt.clientState .^ genRequest slaveDist
      SM.addClientMsg endpoint payload
      SM.simulateN 2
      SM.dropMessages 1
  SM.simulateAll
  Tt.clientState .^ analyzeResponses

-- TODO: maybe we should take statistics on insertion retries. This will help
-- verify PaxosTaskManager and it will help us understand wasted cycles.
test3 :: STS Tt.TestState ()
test3 = do
  Mo.forM_ [1..50] $
    \_ -> do
      Mo.forM_ [1..5] $
        \_ -> do
          (endpoint, payload) <- Tt.clientState .^ genRequest slaveDist
          SM.addClientMsg endpoint payload
      SM.simulateN 2
      SM.dropMessages 2
  SM.simulateAll
  Tt.clientState .^ analyzeResponses

test4 :: STS Tt.TestState ()
test4 = do
  SM.addClientMsg (SM.Master 0) (CRq.CreateDatabase "d" "t" 1); SM.simulateAll
  SM.addClientMsg (SM.Master 0) (CRq.CreateDatabase "d" "t" 2); SM.simulateN 2
  SM.addClientMsg (SM.Slave 0) (CRq.SlaveWrite "d" "t" "key1" "value1" 3); SM.simulateN 2
  SM.addClientMsg (SM.Slave 1) (CRq.SlaveWrite "d" "t" "key2" "value2" 4); SM.simulateN 2
  SM.addClientMsg (SM.Slave 2) (CRq.SlaveWrite "d" "t" "key3" "value3" 5); SM.simulateN 2
  SM.addClientMsg (SM.Slave 3) (CRq.SlaveWrite "d" "t" "key4" "value4" 6); SM.simulateN 2
  SM.addClientMsg (SM.Slave 4) (CRq.SlaveWrite "d" "t" "key5" "value5" 7); SM.simulateAll
  Tt.clientState .^ analyzeResponses

test5 :: STS Tt.TestState ()
test5 = do
  Mo.forM_ [1..50] $
    \_ -> do
      Mo.forM_ [1..5] $
        \_ -> do
          (endpoint, payload) <- Tt.clientState .^ genRequest allRequestsDist
          SM.addClientMsg endpoint payload
      SM.simulateN 2
      SM.dropMessages 2
  SM.simulateAll
  Tt.clientState .^ analyzeResponses

test6 :: STS Tt.TestState ()
test6 = do
  Mo.forM_ [1..50] $
    \_ -> do
      Mo.forM_ [1..5] $
        \_ -> do
          (endpoint, payload) <- Tt.clientState .^ genRequest masterDist
          SM.addClientMsg endpoint payload
      SM.simulateN 2
      SM.dropMessages 2
  SM.simulateAll
  Tt.clientState .^ analyzeResponses

test7 :: STS Tt.TestState ()
test7 = do
  SM.addClientMsg (SM.Master 0) (CRq.CreateDatabase "d" "t1" 1); SM.simulateAll
  SM.addClientMsg (SM.Master 0) (CRq.CreateDatabase "d" "t1" 2); SM.simulateAll
  SM.addClientMsg (SM.Master 0) (CRq.DeleteDatabase "d" "t1" 3); SM.simulateAll
  SM.addClientMsg (SM.Master 0) (CRq.CreateDatabase "d" "t2" 4); SM.simulateAll
  SM.addClientMsg (SM.Slave 0) (CRq.SlaveWrite "d" "t2" "key1" "value1" 5); SM.simulateN 2
  SM.addClientMsg (SM.Slave 1) (CRq.SlaveWrite "d" "t2" "key1" "value2" 6); SM.simulateN 2
  SM.addClientMsg (SM.Slave 2) (CRq.SlaveRead "d" "t2" "key1" 7); SM.simulateAll
  Tt.clientState .^ analyzeResponses

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

driveTest :: Int -> STS Tt.TestState () -> IO ()
driveTest testNum test = do
  let g = SM.createTestState 0 5 5 1
      (_, (_, traceMsgs, g')) = runST test g
      -- We must reverse traceMsgs since that's created in reverse order,
      -- with the most recent message first
      testMsg =
        case verifyTrace $ traceMsgs of
          Left errMsg -> "Test " ++ (show testNum) ++ " Failed! Error: " ++ errMsg
          Right _ -> "Test " ++ (show testNum) ++ " Passed! " ++ (show $ length traceMsgs) ++ " trace messages."
  putStrLn $ ppShow $ g' ^. Tt.clientState . Tt.requestStats
  putStrLn testMsg
  putStrLn ""
  return ()

testDriver :: IO ()
testDriver = do
  driveTest 1 test1
  driveTest 2 test2
  driveTest 3 test3
  driveTest 4 test4
  driveTest 5 test5
  driveTest 6 test6
  driveTest 7 test7

main :: IO ()
main = testDriver
