{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Network where

import qualified Control.Monad as Mo
import qualified Data.Binary as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Mp
import qualified Control.Concurrent.Chan as C
import qualified Control.Concurrent.MVar as MV
import qualified Network.Simple.TCP as TCP
import qualified System.Log.Logger as L

import qualified Message as M

type EndpointId = String
type Connections = Mp.Map EndpointId (C.Chan M.Message)

logM :: String -> IO ()
logM msg = L.logM "network" L.DEBUG msg

addConn
  :: MV.MVar Connections
  -> EndpointId
  -> IO (C.Chan M.Message)
addConn connM endpointId = do
   sendChan <- C.newChan
   conn <- MV.takeMVar connM
   MV.putMVar connM (Mp.insert endpointId sendChan conn)
   return sendChan

delConn
  :: MV.MVar Connections
  -> EndpointId -> IO ()
delConn connM endpointId = do
  conn <- MV.takeMVar connM
  MV.putMVar connM (Mp.delete endpointId conn)

encode :: B.Binary a => a -> BS.ByteString
encode b = BSL.toStrict $ B.encode b

decode :: B.Binary a => BS.ByteString -> a
decode d = B.decode $ BSL.fromStrict d

-- Primitive function for receiving a message
receiveMessage :: B.Binary a => TCP.Socket -> IO a
receiveMessage socket = do
  -- TODO handle socket closure properly
  Just headerB <- TCP.recv socket 8 -- receive fixed-size, 8 byte header
  let msgLen = decode headerB :: Int -- B.decode takes 8 byte ByteStrings and decodes them as Int
  Just bodyB <- TCP.recv socket msgLen
  return $ decode bodyB

-- Primitive function for sending a message
sendMessage :: B.Binary a => TCP.Socket -> a -> IO ()
sendMessage socket msg = do
  let bodyB = encode msg
      headerB = encode $ BS.length bodyB
  TCP.send socket headerB
  TCP.send socket bodyB

-- Thread function for receiving
handleReceive :: B.Binary a => EndpointId -> C.Chan (EndpointId, a) -> TCP.Socket -> IO ()
handleReceive endpointId chan socket = do
  Mo.forever $ do
    message <- receiveMessage socket
    logM "Receiving Message"
    C.writeChan chan (endpointId, message)

-- Thread function for sending
handleSend :: B.Binary a => C.Chan a -> TCP.Socket -> IO ()
handleSend chan socket = do
  Mo.forever $ do
    msg <- C.readChan chan
    logM "Sending Message"
    sendMessage socket msg
