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

mkRow :: String -> Int -> RT.Row
mkRow key value = RT.Row (RT.PrimaryKey [RT.CV'String key]) [Just (RT.CV'Int value)]

-- Tests to see if a row that we write is actually written by trying to
-- read it again. The return value is either a failure message, or the number
-- of checks that succeeded
test1 :: STB Tt.TestState (Either String String)
test1 = do
  let expectedResponses = Mp.empty
  SM.addAdminMsg (SM.mkClientEId 0) (SM.mkServerEId 0) (Ms.Ad'Request' $ Ms.Ad'InsertRq (Co.TabletPath "table1") (mkRow "key1" 2) 1); SM.simulateNms 2
  requestId <- SM.addAdminMsg (SM.mkClientEId 0) (SM.mkServerEId 0) (Ms.Ad'Request' $ Ms.Ad'ReadRowRq (Co.TabletPath "table1") (RT.PrimaryKey [RT.CV'String "key1"]) 1); SM.simulateNms 2
  expectedResponses <- return $ Mp.insert requestId (Ms.Admin $ Ms.Ad'Message (Ms.Ad'Metadata requestId) (Ms.Ad'Response' $ Ms.Ad'ReadRowRs (Just (mkRow "key1" 2)) 1)) expectedResponses
  SM.simulateAll
  msgs <- getL $ Tt.clientMsgsReceived
  let expectedMinusActual = U.fold expectedResponses (Fl.toList msgs) $
        \expectedMinusActual actualMsg ->
          let (Ms.Admin (Ms.Ad'Message (Ms.Ad'Metadata requestId) _)) = actualMsg
          in case Mp.lookup requestId expectedMinusActual of
            Just msg | msg == actualMsg -> Mp.delete requestId expectedMinusActual
            _ -> expectedMinusActual
  if (Mp.size expectedMinusActual) == 0
    then return $ Right $ show (Mp.size expectedResponses) ++ " checks made."
    else return $ Left $ "Expected " ++ show (Mp.size expectedResponses) ++
                         " responses to be received, but " ++ show (Mp.size expectedMinusActual) ++
                         " of those still weren't."

driveTest :: Int -> Int -> STB Tt.TestState (Either String String) -> IO ()
driveTest seed testNum test = do
  let g = SM.createTestState seed 5 1
      (result, (_, _, g')) = runST test1 g
      testMsg =
        case result of
          Left errMsg -> "Test " ++ (show testNum) ++ " Failed! Error: " ++ errMsg
          Right succMsg -> "Test " ++ (show testNum) ++ " Passed! " ++ succMsg
  putStrLn testMsg
  return ()

testDriver :: IO ()
testDriver = do
  driveTest 0 1 test1

main :: IO ()
main = testDriver
