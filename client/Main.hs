{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Control.Concurrent as Ct
import qualified Control.Monad as Mo
import qualified Network.Simple.TCP as TCP
import qualified System.Environment as SE

import qualified Connections as Cn
import qualified Infra.Logging as Lg
import qualified Proto.Messages.ClientMessages as CM
import qualified Proto.Messages as Ms

-- Example commands: r k 0, w k v 0
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
        ["r", key, timestamp] -> do
          Cn.sendMessage socket $ Ms.ClientRequest $ CM.ReadRequest key (read timestamp)
        ["w", key, value, timestamp] -> do
          Cn.sendMessage socket $ Ms.ClientRequest $ CM.WriteRequest key value (read timestamp)
        _ -> print "Unrecognized command or number of arguments"

main :: IO ()
main = do
  Lg.setupLogging
  args <- SE.getArgs
  startClient args
