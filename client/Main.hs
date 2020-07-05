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
import qualified Proto.Messages as Ms
import qualified Proto.Messages.ClientRequests as CRq

-- Example commands:
-- r d t k 0
-- w d t k v 0
-- c d t
startClient :: [String] -> IO ()
startClient (ip:message) = do
  Lg.infoM Lg.main "Starting client"
  TCP.connect ip "9000" $ \(socket, remoteAddr) -> do
    Lg.infoM Lg.main $ "Connection established to " ++ show remoteAddr
    Ct.forkIO $ Mo.forever $ do
      msg <- Cn.receiveMessage socket
      print (msg :: Ms.Message)
    Mo.forever $ do
      line <- getLine
      case words line of
        ["r", databaseId, tableId, key, timestamp] -> do
          Cn.sendMessage socket $
            Ms.ClientRequest 
              (CRq.ClientRequest
                (CRq.RequestMeta "uid")
                (CRq.ReadRequest databaseId tableId key (read timestamp)))
        ["w", databaseId, tableId, key, value, timestamp] -> do
          Cn.sendMessage socket $
            Ms.ClientRequest 
              (CRq.ClientRequest
                (CRq.RequestMeta "uid")
                (CRq.WriteRequest databaseId tableId key value (read timestamp)))
        ["c", databaseId, tableId] -> do
          Cn.sendMessage socket $
            Ms.ClientRequest
              (CRq.ClientRequest
                (CRq.RequestMeta "uid")
                (CRq.CreateDatabase databaseId tableId))
        _ -> print "Unrecognized command or number of arguments"

main :: IO ()
main = do
  Lg.setupLogging
  args <- SE.getArgs
  startClient args
