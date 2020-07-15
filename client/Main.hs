{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Concurrent as Ct
import qualified Control.Monad as Mo
import qualified Network.Simple.TCP as TCP
import qualified System.Environment as SE

import qualified Infra.Logging as Lg
import qualified Infra.Utils as U
import qualified Net.Connections as Cn
import qualified Proto.Common as Co
import qualified Proto.Messages as Ms
import qualified Proto.Messages.ClientRequests as CRq
import Infra.State

-- Example commands:
-- r d t k 0
-- w d t k v 0
-- wr d t 1
-- c d t 1
startClient :: String -> String -> IO ()
startClient mip sip = do
  Lg.infoM Lg.main "Starting client"
  TCP.connect mip "8000" $ \(masterSocket, remoteAddr) -> do
    Lg.infoM Lg.main $ "Connection established to " ++ show remoteAddr
    Ct.forkIO $ Mo.forever $ do
      msg :: Ms.Message <- Cn.receiveMessage masterSocket
      putStrLn $ ppShow msg
    TCP.connect sip "8000" $ \(slaveSocket, remoteAddr) -> do
      Lg.infoM Lg.main $ "Connection established to " ++ show remoteAddr
      Ct.forkIO $ Mo.forever $ do
        msg :: Ms.Message <- Cn.receiveMessage slaveSocket
        putStrLn $ ppShow msg
      let loop ranges count = do
            line <- getLine
            let uid = "uid" ++ show count
            ranges' <-
              case words line of
                ["r", databaseId, tableId, key, timestamp] -> do
                  Cn.sendMessage slaveSocket $
                    Ms.ClientRequest
                      (CRq.ClientRequest
                        (CRq.Meta uid)
                        (CRq.SlaveRead databaseId tableId key (read timestamp)))
                  return ranges
                ["w", databaseId, tableId, key, value, timestamp] -> do
                  Cn.sendMessage slaveSocket $
                    Ms.ClientRequest
                      (CRq.ClientRequest
                        (CRq.Meta uid)
                        (CRq.SlaveWrite databaseId tableId key value (read timestamp)))
                  return ranges
                ["wr", databaseId, tableId, timestamp] -> do
                  let ranges' = ((Co.KeySpaceRange databaseId tableId):ranges)
                  Cn.sendMessage slaveSocket $
                    Ms.ClientRequest
                      (CRq.ClientRequest
                        (CRq.Meta uid)
                        (CRq.RangeWrite ranges' (read timestamp)))
                  return ranges'
                ["c", databaseId, tableId, timestamp] -> do
                  let ranges' = ((Co.KeySpaceRange databaseId tableId):ranges)
                  Cn.sendMessage masterSocket $
                    Ms.ClientRequest
                      (CRq.ClientRequest
                        (CRq.Meta uid)
                        (CRq.CreateDatabase databaseId tableId (read timestamp)))
                  return ranges'
                _ -> do
                  putStrLn $ ppShow "Unrecognized command or number of arguments"
                  return ranges
            loop ranges' (count + 1)
      loop [] 0

main :: IO ()
main = do
  Lg.setupLogging
  args <- SE.getArgs
  let (mip:sip:_) = args
  startClient mip sip
