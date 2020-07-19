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

addGenRequest
  :: Co.EndpointId
  -> CS.RequestTypeDist
  -> STS Tt.TestState ()
addGenRequest fromEId requestDist = do
    trueTimestamp <- getL $ Tt.trueTimestamp
    (toEId, payload) <- Tt.clientState . ix fromEId .^* CS.genRequest trueTimestamp requestDist
    SM.addClientMsg fromEId toEId payload

setupInitialDBs :: STS Tt.TestState ()
setupInitialDBs = do
  clientEIds <- getL $ Tt.clientEIds
  -- First, fully create a few databases to help improve liveness of the test
  let fromEId = SM.mkClientEId 0
  Mo.forM_ [1..5] $ \_ -> do
    addGenRequest fromEId CS.createDatabaseDist
    SM.simulateAll
  -- Next, make sure the clients know about each database.
  Mo.forM_ clientEIds $ \fromEId -> do
    addGenRequest fromEId CS.rangeReadDist
    SM.simulateAll

test1 :: STS Tt.TestState ()
test1 = do
  SM.addClientMsg (SM.mkClientEId 0) (SM.mkSlaveEId 0 0) (CRq.RangeWrite [Co.KeySpaceRange (Co.DatabaseId "d") (Co.TableId "t")] 1); SM.simulateNms 2
  SM.addClientMsg (SM.mkClientEId 0) (SM.mkSlaveEId 0 1) (CRq.SlaveWrite (Co.DatabaseId "d") (Co.TableId "t") "key1" "value1" 2); SM.simulateNms 2
  SM.addClientMsg (SM.mkClientEId 0) (SM.mkSlaveEId 0 2) (CRq.SlaveWrite (Co.DatabaseId "d") (Co.TableId "t") "key2" "value2" 3); SM.simulateNms 2
  SM.addClientMsg (SM.mkClientEId 0) (SM.mkSlaveEId 0 3) (CRq.SlaveWrite (Co.DatabaseId "d") (Co.TableId "t") "key3" "value3" 4); SM.simulateNms 2
  SM.addClientMsg (SM.mkClientEId 0) (SM.mkSlaveEId 0 4) (CRq.SlaveWrite (Co.DatabaseId "d") (Co.TableId "t") "key4" "value4" 5); SM.simulateNms 2
  SM.addClientMsg (SM.mkClientEId 0) (SM.mkSlaveEId 0 0) (CRq.SlaveWrite (Co.DatabaseId "d") (Co.TableId "t") "key5" "value5" 6); SM.simulateAll

-- Tasks 100ms of execution at 1 requests per second, where
-- the requests that are sent are only slave requests.
test2 :: STS Tt.TestState ()
test2 = do
  setupInitialDBs
  clientEIds <- getL $ Tt.clientEIds
  Mo.forM_ [1..100] $
    \_ -> do
      fromEId <- Tt.rand .^^ U.randomL clientEIds
      addGenRequest fromEId CS.slaveDist
      SM.simulate1ms
      SM.dropMessages 1
  SM.simulateAll

-- Tasks 50ms of execution at 5 requests per second, where
-- the requests that are sent are only slave requests.
test3 :: STS Tt.TestState ()
test3 = do
  setupInitialDBs
  clientEIds <- getL $ Tt.clientEIds
  Mo.forM_ [1..50] $
    \_ -> do
      Mo.forM_ [1..5] $
        \_ -> do
          fromEId <- Tt.rand .^^ U.randomL clientEIds
          addGenRequest fromEId CS.slaveDist
      SM.simulate1ms
      SM.dropMessages 2
  SM.simulateAll

test4 :: STS Tt.TestState ()
test4 = do
  SM.addClientMsg (SM.mkClientEId 0) (SM.mkMasterEId 0) (CRq.CreateDatabase (Co.DatabaseId "d") (Co.TableId "t") 1); SM.simulateAll
  SM.addClientMsg (SM.mkClientEId 0) (SM.mkMasterEId 0) (CRq.CreateDatabase (Co.DatabaseId "d") (Co.TableId "t") 2); SM.simulateNms 2
  SM.addClientMsg (SM.mkClientEId 0) (SM.mkSlaveEId 0 0) (CRq.SlaveWrite (Co.DatabaseId "d") (Co.TableId "t") "key1" "value1" 3); SM.simulateNms 2
  SM.addClientMsg (SM.mkClientEId 0) (SM.mkSlaveEId 0 1) (CRq.SlaveWrite (Co.DatabaseId "d") (Co.TableId "t") "key2" "value2" 4); SM.simulateNms 2
  SM.addClientMsg (SM.mkClientEId 0) (SM.mkSlaveEId 0 2) (CRq.SlaveWrite (Co.DatabaseId "d") (Co.TableId "t") "key3" "value3" 5); SM.simulateNms 2
  SM.addClientMsg (SM.mkClientEId 0) (SM.mkSlaveEId 0 3) (CRq.SlaveWrite (Co.DatabaseId "d") (Co.TableId "t") "key4" "value4" 6); SM.simulateNms 2
  SM.addClientMsg (SM.mkClientEId 0) (SM.mkSlaveEId 0 4) (CRq.SlaveWrite (Co.DatabaseId "d") (Co.TableId "t") "key5" "value5" 7); SM.simulateAll

-- Tasks 50ms of execution at 5 requests per second, where
-- the requests that are all possible requests.
test5 :: STS Tt.TestState ()
test5 = do
  setupInitialDBs
  clientEIds <- getL $ Tt.clientEIds
  Mo.forM_ [1..50] $
    \_ -> do
      Mo.forM_ [1..5] $
        \_ -> do
          fromEId <- Tt.rand .^^ U.randomL clientEIds
          addGenRequest fromEId CS.allRequestsDist
      SM.simulate1ms
      SM.dropMessages 2
  SM.simulateAll

-- Tasks 50ms of execution at 5 requests per second, where
-- the requests that are only master requests.
test6 :: STS Tt.TestState ()
test6 = do
  setupInitialDBs
  clientEIds <- getL $ Tt.clientEIds
  Mo.forM_ [1..50] $
    \_ -> do
      Mo.forM_ [1..5] $
        \_ -> do
          fromEId <- Tt.rand .^^ U.randomL clientEIds
          addGenRequest fromEId CS.masterDist
      SM.simulate1ms
      SM.dropMessages 2
  SM.simulateAll


test7 :: STS Tt.TestState ()
test7 = do
  SM.addClientMsg (SM.mkClientEId 0) (SM.mkMasterEId 0) (CRq.CreateDatabase (Co.DatabaseId "d") (Co.TableId "t1") 1); SM.simulateAll
  SM.addClientMsg (SM.mkClientEId 0) (SM.mkMasterEId 0) (CRq.CreateDatabase (Co.DatabaseId "d") (Co.TableId "t1") 2); SM.simulateAll
  SM.addClientMsg (SM.mkClientEId 0) (SM.mkMasterEId 0) (CRq.DeleteDatabase (Co.DatabaseId "d") (Co.TableId "t1") 3); SM.simulateAll
  SM.addClientMsg (SM.mkClientEId 0) (SM.mkMasterEId 0) (CRq.CreateDatabase (Co.DatabaseId "d") (Co.TableId "t2") 4); SM.simulateAll
  SM.addClientMsg (SM.mkClientEId 0) (SM.mkSlaveEId 0 0) (CRq.SlaveWrite (Co.DatabaseId "d") (Co.TableId "t2") "key1" "value1" 5); SM.simulateNms 2
  SM.addClientMsg (SM.mkClientEId 0) (SM.mkSlaveEId 0 1) (CRq.SlaveWrite (Co.DatabaseId "d") (Co.TableId "t2") "key1" "value2" 6); SM.simulateNms 2
  SM.addClientMsg (SM.mkClientEId 0) (SM.mkSlaveEId 0 2) (CRq.SlaveRead (Co.DatabaseId "d") (Co.TableId "t2") "key1" 7); SM.simulateAll

-- Stress test with more requests
test8 :: STS Tt.TestState ()
test8 = do
  setupInitialDBs
  clientEIds <- getL $ Tt.clientEIds
  Mo.forM_ [1..200] $
    \_ -> do
      Mo.forM_ [1..5] $
        \_ -> do
          fromEId <- Tt.rand .^^ U.randomL clientEIds
          addGenRequest fromEId CS.allRequestsDist
      SM.simulate1ms
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

driveTest :: Int -> Int -> STS Tt.TestState () -> IO ()
driveTest seed testNum test = do
  let g = SM.createTestState seed 5 5
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
  driveTest 0 1 test1
  driveTest 0 2 test2
  driveTest 0 3 test3
  driveTest 0 4 test4
  driveTest 0 5 test5
  driveTest 0 6 test6
  driveTest 0 7 test7
  driveTest 0 8 test8
  driveTest 1 8 test8
  driveTest 2 8 test8

main :: IO ()
main = testDriver
