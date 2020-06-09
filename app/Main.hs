{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import qualified Network.Simple.TCP as TCP
import qualified Data.ByteString as BS
import qualified System.Environment as E
import Lib

import qualified Data.Default as D
import qualified GHC.Generics as G
import Data.Binary (Word8)

data Test = Test Word8 deriving (G.Generic, D.Default)

startServer :: IO ()
startServer = do
  print "Starting Server"
  TCP.serve (TCP.Host "127.0.0.1") "8000" $ \(socket, remoteAddr) -> do
    putStrLn $ "TCP connection established from " ++ show remoteAddr
    (Just req) <- TCP.recv socket 100
    putStrLn $ show $ BS.unpack req
    let test = D.def :: Test
        Test val = test
        res = BS.pack [3, 2, 1, val]
    TCP.send socket res

startClient :: IO ()
startClient = do
  print "Starting Client"
  TCP.connect "127.0.0.1" "8000" $ \(socket, remoteAddr) -> do
    putStrLn $ "Connection established to " ++ show remoteAddr
    let req = BS.pack [1, 2, 3]
    TCP.send socket req
    (Just res) <- TCP.recv socket 100
    putStrLn $ show $ BS.unpack res

main :: IO ()
main = do
  args <- E.getArgs
  let (mode:_) = args
  if mode == "server"
    then startServer
    else startClient
