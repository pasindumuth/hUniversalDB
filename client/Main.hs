{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Control.Concurrent as Ct
import qualified Control.Monad as Mo
import qualified Network.Simple.TCP as TCP
import qualified System.Environment as SE

import qualified Infra.Logging as Lg
import qualified Net.Connections as Cn
import qualified Proto.Common as Co
import qualified Proto.Messages as Ms
import qualified Proto.Messages.ClientRequests as CRq

-- Example commands:
-- r d t k 0
-- w d t k v 0
-- c 0 d t
startClient :: String -> IO ()
startClient ip = do
  Lg.infoM Lg.main "Starting client"
  TCP.connect ip "9000" $ \(socket, remoteAddr) -> do
    Lg.infoM Lg.main $ "Connection established to " ++ show remoteAddr
    Ct.forkIO $ Mo.forever $ do
      msg <- Cn.receiveMessage socket
      print (msg :: Ms.Message)
    loop socket []
  where
    loop :: TCP.Socket -> [Co.KeySpaceRange] -> IO ()
    loop socket ranges = do
      line <- getLine
      ranges' <-
        case words line of
          ["r", databaseId, tableId, key, timestamp] -> do
            Cn.sendMessage socket $
              Ms.ClientRequest
                (CRq.ClientRequest
                  (CRq.Meta "uid")
                  (CRq.SlaveRead databaseId tableId key (read timestamp)))
            return ranges
          ["w", databaseId, tableId, key, value, timestamp] -> do
            Cn.sendMessage socket $
              Ms.ClientRequest
                (CRq.ClientRequest
                  (CRq.Meta "uid")
                  (CRq.SlaveWrite databaseId tableId key value (read timestamp)))
            return ranges
          ["c", databaseId, tableId, timestamp] -> do
            let ranges' = ((Co.KeySpaceRange databaseId tableId):ranges)
            Cn.sendMessage socket $
              Ms.ClientRequest
                (CRq.ClientRequest
                  (CRq.Meta "uid")
                  (CRq.RangeWrite ranges' (read timestamp)))
            return ranges'
          _ -> do
            print "Unrecognized command or number of arguments"
            return ranges
      loop socket ranges'

main :: IO ()
main = do
  Lg.setupLogging
  args <- SE.getArgs
  let (ip:_) = args
  startClient ip
