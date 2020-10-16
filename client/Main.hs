{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Concurrent as Ct
import qualified Control.Monad as Mo
import qualified Data.List as Li
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
-- d d t 1
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
            let requestId = Co.RequestId $ "requestId" ++ show count
            ranges' <-
              case words line of
                ["r", path, key, timestamp] -> do
                  Cn.sendMessage slaveSocket $
                    Ms.ClientRequest
                      (CRq.ClientRequest
                        (CRq.Meta requestId)
                        (CRq.SlaveRead
                          (Co.Path path)
                          key
                          (read timestamp)))
                  return ranges
                ["w", path, key, value, timestamp] -> do
                  Cn.sendMessage slaveSocket $
                    Ms.ClientRequest
                      (CRq.ClientRequest
                        (CRq.Meta requestId)
                        (CRq.SlaveWrite
                          (Co.Path path)
                          key
                          value
                          (read timestamp)))
                  return ranges
                ["wr", path, timestamp] -> do
                  let ranges' = ((Co.KeySpaceRange (Co.Path path)):ranges)
                  Cn.sendMessage slaveSocket $
                    Ms.ClientRequest
                      (CRq.ClientRequest
                        (CRq.Meta requestId)
                        (CRq.RangeWrite ranges' (read timestamp)))
                  return ranges'
                ["c", path, timestamp] -> do
                  let ranges' = ((Co.KeySpaceRange (Co.Path path)):ranges)
                  Cn.sendMessage masterSocket $
                    Ms.ClientRequest
                      (CRq.ClientRequest
                        (CRq.Meta requestId)
                        (CRq.CreateDatabase
                          (Co.Path path)
                          (read timestamp)))
                  return ranges'
                ["d", path, timestamp] -> do
                  let ranges' = Li.delete (Co.KeySpaceRange (Co.Path path)) ranges
                  Cn.sendMessage masterSocket $
                    Ms.ClientRequest
                      (CRq.ClientRequest
                        (CRq.Meta requestId)
                        (CRq.DeleteDatabase
                          (Co.Path path)
                          (read timestamp)))
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
