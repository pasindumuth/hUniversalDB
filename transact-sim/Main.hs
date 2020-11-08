module Main where

import qualified Data.Foldable as Fl
import qualified Data.Map as Mp
import qualified Data.Sequence as Sq

import qualified Common.Model.RelationalTablet as RT
import qualified Infra.Utils as U
import qualified SimulationManager as SM
import qualified TestState as Tt
import qualified Transact.Model.Common as Co
import qualified Transact.Model.Message as Ms
import Infra.Lens
import Transact.Infra.State

------------------------------------------------------------------------------------------------------------------------
-- Testing Utilities
------------------------------------------------------------------------------------------------------------------------

-- | Hard simple coded table schema we will use for all tables.
schema :: RT.Schema
schema = RT.Schema [("key", RT.CT'String)] [("value", RT.CT'Int)]

-- | A simpe utility for making Co.TabletKeyRanges for the specific schema above
mkKeyRange :: Maybe String -> Maybe String -> Co.TabletKeyRange
mkKeyRange startKeyM endKeyM =
  Co.TabletKeyRange (fmap toPrimary startKeyM) (fmap toPrimary endKeyM)
  where
    toPrimary key = RT.PrimaryKey [RT.CV'String key]

-- | Hard coded table partitioning configurations. We only handle 5 servers and
-- only a handful of tables. We don't support SQL DDL yet.
partitionConfig :: Mp.Map Co.EndpointId [Co.TabletShape]
partitionConfig = Mp.fromList [
  (Co.EndpointId "s0", [
    Co.TabletShape (Co.TabletPath "table1") (mkKeyRange Nothing Nothing)]),
  (Co.EndpointId "s1", [
    Co.TabletShape (Co.TabletPath "table2") (mkKeyRange Nothing (Just "j"))]),
  (Co.EndpointId "s2", [
    Co.TabletShape (Co.TabletPath "table2") (mkKeyRange (Just "j") Nothing),
    Co.TabletShape (Co.TabletPath "table3") (mkKeyRange Nothing (Just "d")),
    Co.TabletShape (Co.TabletPath "table4") (mkKeyRange Nothing (Just "k"))]),
  (Co.EndpointId "s3", [
    Co.TabletShape (Co.TabletPath "table3") (mkKeyRange (Just "d") (Just "p"))]),
  (Co.EndpointId "s4", [
    Co.TabletShape (Co.TabletPath "table3") (mkKeyRange (Just "p") Nothing),
    Co.TabletShape (Co.TabletPath "table4") (mkKeyRange (Just "k") Nothing)])]

mkKey :: String -> RT.PrimaryKey
mkKey key = (RT.PrimaryKey [RT.CV'String key])

mkRow :: String -> Int -> RT.Row
mkRow key value = RT.Row (mkKey key) [Just (RT.CV'Int value)]

-- Add an Admin Request message between two nodes, and set a
-- response that should occur anytime in the future
addReqRes
  :: Co.EndpointId -- ^ The node to send from
  -> Co.EndpointId -- ^ The node to send to
  -> Ms.Ad'Payload -- ^ The request payload
  -> Ms.Ad'Payload -- ^ The response payload
  -> (Mp.Map Co.RequestId Ms.Message) -- ^ All responses that are currently being expected to occur.
  -> STB Tt.TestState (Mp.Map Co.RequestId Ms.Message) -- ^ The new set of responses to be expected.
addReqRes fromEId toEId requestPaylod responsePayload expectedResponses = do
  requestId <- SM.addAdminMsg fromEId toEId requestPaylod
  let response = Ms.Admin $ Ms.Ad'Message (Ms.Ad'Metadata requestId) responsePayload
  return $ Mp.insert requestId response expectedResponses

-- Checks to see if the expected responses actually occurred. We
-- get the actual responses that were received by clients with the
-- Tt.clientMsgsReceived of Tt.TestState.
checkResponses
  :: (Mp.Map Co.RequestId Ms.Message) -- ^ All responses that are currently being expected to occur.
  -> STB Tt.TestState (Either String String) -- ^ A success message if all responses occurred, or a failure otherwise
checkResponses expectedResponses = do
    msgs <- getL $ Tt.clientMsgsReceived
    let expectedMinusActual = U.fold expectedResponses (Fl.toList msgs) $
          \expectedMinusActual actualMsg ->
            let (Ms.Admin (Ms.Ad'Message (Ms.Ad'Metadata requestId) _)) = actualMsg
            in case Mp.lookup requestId expectedMinusActual of
              Just msg | msg == actualMsg -> Mp.delete requestId expectedMinusActual
              _ -> expectedMinusActual
    if Mp.size expectedMinusActual == 0
      then return $ Right $ show (Mp.size expectedResponses) ++ " checks made."
      else return $ Left $ "Expected " ++ show (Mp.size expectedResponses) ++
                           " responses to be received, but " ++ show (Mp.size expectedMinusActual) ++
                           " of those still weren't."

------------------------------------------------------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------------------------------------------------------
-- | Tests to see if a row that we write is actually written by trying to
-- read it again. The return value is either a failure message, or the number
-- of checks that succeeded
test1 :: STB Tt.TestState (Either String String)
test1 = do
  let expectedResponses = Mp.empty
  -- Insert a row
  SM.addAdminMsg (SM.mkClientEId 0) (SM.mkServerEId 0) $
    (Ms.Ad'Request'
      (Ms.Ad'InsertRq
        (Co.TabletPath "table1")
        (mkRow "key1" 2)
        1))
  SM.simulateNms 2
  -- Read the row and expect a response
  expectedResponses <- addReqRes (SM.mkClientEId 0) (SM.mkServerEId 0)
    (Ms.Ad'Request' (
      Ms.Ad'ReadRowRq
        (Co.TabletPath "table1")
        (mkKey "key1")
        1))
    (Ms.Ad'Response' (
      Ms.Ad'ReadRowRs
        (Just (mkRow "key1" 2))
        1))
    expectedResponses
  SM.simulateAll
  -- See if we get expected responses
  checkResponses expectedResponses

-- | Tests to see if a row that we write is actually written by trying to
-- read it again. The return value is either a failure message, or the number
-- of checks that succeeded
test2 :: STB Tt.TestState (Either String String)
test2 = do
  let expectedResponses = Mp.empty
  -- Insert a row
  SM.addAdminMsg (SM.mkClientEId 0) (SM.mkServerEId 0) $
    (Ms.Ad'Request'
      (Ms.Ad'InsertRq
        (Co.TabletPath "table1")
        (mkRow "key1" 2)
        1))
  SM.simulateNms 2
  -- Update the row
  SM.addAdminMsg (SM.mkClientEId 0) (SM.mkServerEId 0) $
    (Ms.Ad'Request'
      (Ms.Ad'UpdateRq
        (Co.TabletPath "table1")
        (mkKey "key1")
        "value"
        (Just $ RT.CV'Int 3)
        2))
  SM.simulateNms 2
  -- Read the row and expect a response
  expectedResponses <- addReqRes (SM.mkClientEId 0) (SM.mkServerEId 0)
    (Ms.Ad'Request' (
      Ms.Ad'ReadRowRq
        (Co.TabletPath "table1")
        (mkKey "key1")
        1))
    (Ms.Ad'Response' (
      Ms.Ad'ReadRowRs
        (Just (mkRow "key1" 2))
        1))
    expectedResponses
  -- Read the row and expect a response
  expectedResponses <- addReqRes (SM.mkClientEId 0) (SM.mkServerEId 0)
    (Ms.Ad'Request' (
      Ms.Ad'ReadRowRq
        (Co.TabletPath "table1")
        (mkKey "key1")
        2))
    (Ms.Ad'Response' (
      Ms.Ad'ReadRowRs
        (Just (mkRow "key1" 3))
        2))
    expectedResponses
  SM.simulateAll
  -- See if we get expected responses
  checkResponses expectedResponses

------------------------------------------------------------------------------------------------------------------------
-- Test driver
------------------------------------------------------------------------------------------------------------------------
driveTest :: Int -> Int -> STB Tt.TestState (Either String String) -> IO ()
driveTest seed testNum test = do
  let g = SM.createTestState seed 5 1 partitionConfig schema
      (result, (_, _, g')) = runST test g
      testMsg =
        case result of
          Left errMsg -> "Test " ++ (show testNum) ++ " Failed! Error: " ++ errMsg
          Right succMsg -> "Test " ++ (show testNum) ++ " Passed! " ++ succMsg
  putStrLn testMsg
  return ()

testDriver :: IO ()
testDriver = do
  driveTest 0 1 test1
  driveTest 0 2 test2

main :: IO ()
main = testDriver
