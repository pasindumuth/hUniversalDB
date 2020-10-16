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
import Infra.State

startClient :: String -> String -> IO ()
startClient mip sip = do
  Lg.infoM Lg.main "Starting client"
  TCP.connect sip "8000" $ \(slaveSocket, remoteAddr) -> do
    Lg.infoM Lg.main $ "Connection established to " ++ show remoteAddr
    Ct.forkIO $ Mo.forever $ do
      msg :: Ms.Message <- Cn.receiveMessage slaveSocket
      putStrLn $ ppShow msg
    return ()

main :: IO ()
main = do
  Lg.setupLogging
  args <- SE.getArgs
  let (mip:sip:_) = args
  startClient mip sip
