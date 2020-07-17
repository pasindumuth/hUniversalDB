{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Monad as Mo
import qualified Data.Map as Mp
import qualified Data.Set as St
import qualified System.Random as Rn

import qualified ClientState as CS
import qualified Infra.Utils as U
import qualified Proto.Common as Co
import qualified Proto.Messages as Ms
import qualified Proto.Messages.ClientRequests as CRq
import qualified Proto.Messages.ClientResponses as CRs
import qualified Proto.Messages.TraceMessages as TrM
import qualified SimulationManager as SM
import qualified TestState as Tt
import qualified TraceChecker as TC
import Infra.Lens
import Infra.State

--test1 :: STS Tt.TestState ()
--test1 = do
--  SM.addClientMsg (SM.Slave 0) (CRq.RangeWrite [Co.KeySpaceRange "d" "t"] 1); SM.simulateN 2
--  SM.addClientMsg (SM.Slave 1) (CRq.SlaveWrite "d" "t" "key1" "value1" 2); SM.simulateN 2
--  SM.addClientMsg (SM.Slave 2) (CRq.SlaveWrite "d" "t" "key2" "value2" 3); SM.simulateN 2
--  SM.addClientMsg (SM.Slave 3) (CRq.SlaveWrite "d" "t" "key3" "value3" 4); SM.simulateN 2
--  SM.addClientMsg (SM.Slave 4) (CRq.SlaveWrite "d" "t" "key4" "value4" 5); SM.simulateN 2
--  SM.addClientMsg (SM.Slave 0) (CRq.SlaveWrite "d" "t" "key5" "value5" 6); SM.simulateAll
--
--test2 :: STS Tt.TestState ()
--test2 = do
--  Mo.forM_ [1..100] $
--    \_ -> do
--      (endpoint, payload) <- Tt.clientState .^ genRequest slaveDist
--      SM.addClientMsg endpoint payload
--      SM.simulateN 2
--      SM.dropMessages 1
--  SM.simulateAll
--
---- TODO: maybe we should take statistics on insertion retries. This will help
---- verify PaxosTaskManager and it will help us understand wasted cycles.
--test3 :: STS Tt.TestState ()
--test3 = do
--  Mo.forM_ [1..50] $
--    \_ -> do
--      Mo.forM_ [1..5] $
--        \_ -> do
--          (endpoint, payload) <- Tt.clientState .^ genRequest slaveDist
--          SM.addClientMsg endpoint payload
--      SM.simulateN 2
--      SM.dropMessages 2
--  SM.simulateAll

--test4 :: STS Tt.TestState ()
--test4 = do
--  SM.addClientMsg (SM.Master 0) (CRq.CreateDatabase "d" "t" 1); SM.simulateAll
--  SM.addClientMsg (SM.Master 0) (CRq.CreateDatabase "d" "t" 2); SM.simulateN 2
--  SM.addClientMsg (SM.Slave 0) (CRq.SlaveWrite "d" "t" "key1" "value1" 3); SM.simulateN 2
--  SM.addClientMsg (SM.Slave 1) (CRq.SlaveWrite "d" "t" "key2" "value2" 4); SM.simulateN 2
--  SM.addClientMsg (SM.Slave 2) (CRq.SlaveWrite "d" "t" "key3" "value3" 5); SM.simulateN 2
--  SM.addClientMsg (SM.Slave 3) (CRq.SlaveWrite "d" "t" "key4" "value4" 6); SM.simulateN 2
--  SM.addClientMsg (SM.Slave 4) (CRq.SlaveWrite "d" "t" "key5" "value5" 7); SM.simulateAll
--
--test5 :: STS Tt.TestState ()
--test5 = do
--  Mo.forM_ [1..50] $
--    \_ -> do
--      Mo.forM_ [1..5] $
--        \_ -> do
--          (endpoint, payload) <- Tt.clientState .^ genRequest allRequestsDist
--          SM.addClientMsg endpoint payload
--      SM.simulateN 2
--      SM.dropMessages 2
--  SM.simulateAll
--
--test6 :: STS Tt.TestState ()
--test6 = do
--  Mo.forM_ [1..50] $
--    \_ -> do
--      Mo.forM_ [1..5] $
--        \_ -> do
--          (endpoint, payload) <- Tt.clientState .^ genRequest masterDist
--          SM.addClientMsg endpoint payload
--      SM.simulateN 2
--      SM.dropMessages 2
--  SM.simulateAll
--
--test7 :: STS Tt.TestState ()
--test7 = do
--  SM.addClientMsg (SM.Master 0) (CRq.CreateDatabase "d" "t1" 1); SM.simulateAll
--  SM.addClientMsg (SM.Master 0) (CRq.CreateDatabase "d" "t1" 2); SM.simulateAll
--  SM.addClientMsg (SM.Master 0) (CRq.DeleteDatabase "d" "t1" 3); SM.simulateAll
--  SM.addClientMsg (SM.Master 0) (CRq.CreateDatabase "d" "t2" 4); SM.simulateAll
--  SM.addClientMsg (SM.Slave 0) (CRq.SlaveWrite "d" "t2" "key1" "value1" 5); SM.simulateN 2
--  SM.addClientMsg (SM.Slave 1) (CRq.SlaveWrite "d" "t2" "key1" "value2" 6); SM.simulateN 2
--  SM.addClientMsg (SM.Slave 2) (CRq.SlaveRead "d" "t2" "key1" 7); SM.simulateAll

test0 :: STS Tt.TestState ()
test0 = do
  clientEIds <- getL $ Tt.clientEIds
  -- First, fully create a few databases to help improve liveness of the test
  let fromEId = SM.mkClientEId 0
  Mo.forM_ [1..5] $ \_ -> do
    trueTimestamp <- getL $ Tt.trueTimestamp
    (toEId, payload) <- Tt.clientState . ix fromEId .^* CS.genRequest trueTimestamp CS.createDatabaseDist
    SM.addClientMsg fromEId toEId payload
    SM.simulateAll
  -- Next, Make sure e
  Mo.forM_ clientEIds $ \fromEId -> do
    trueTimestamp <- getL $ Tt.trueTimestamp
    (toEId, payload) <- Tt.clientState . ix fromEId .^* CS.genRequest trueTimestamp CS.rangeReadDist
    SM.addClientMsg fromEId toEId payload
    SM.simulateAll
  -- Finally, randomly pick clients to run, and run them.
  Mo.forM_ [1..200] $
    \_ -> do
      Mo.forM_ [1..5] $
        \_ -> do
          fromEId <- Tt.rand .^^ U.randomL clientEIds
          trueTimestamp <- getL $ Tt.trueTimestamp
          (toEId, payload) <- Tt.clientState . ix fromEId .^* CS.genRequest trueTimestamp CS.allRequestsDist
          SM.addClientMsg fromEId toEId payload
      SM.simulateN 2
      SM.dropMessages 2
  SM.simulateAll

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
  putStrLn $ ppShow $ g' ^. Tt.requestStats
  putStrLn testMsg
  putStrLn ""
  return ()

testDriver :: IO ()
testDriver = do
  driveTest 0 test0
--  driveTest 1 test1
--  driveTest 2 test2
--  driveTest 3 test3
--  driveTest 4 test4
--  driveTest 5 test5
--  driveTest 6 test6
--  driveTest 7 test7

main :: IO ()
main = testDriver
