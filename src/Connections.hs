{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Connections where

import qualified Control.Monad as Mo
import qualified Data.Binary as Bn
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Mp
import qualified Control.Concurrent.Chan as C
import qualified Control.Concurrent.MVar as MV
import qualified Network.Simple.TCP as TCP

import qualified Logging as Lg
import qualified Records.Messages.Messages as Ms

type EndpointId = String
type Connections = Mp.Map EndpointId (Ms.Message -> IO ())

-- remove from common code; only EndpointId should be common

addConn
  :: MV.MVar Connections
  -> EndpointId
  -> IO (C.Chan Ms.Message)
addConn connM endpointId = do
   sendChan <- C.newChan
   conn <- MV.takeMVar connM
   MV.putMVar connM (Mp.insert endpointId (C.writeChan sendChan) conn)
   return sendChan

delConn
  :: MV.MVar Connections
  -> EndpointId
  -> IO ()
delConn connM endpointId = do
  conn <- MV.takeMVar connM
  MV.putMVar connM (Mp.delete endpointId conn)

encode :: Bn.Binary a => a -> BS.ByteString
encode b = BSL.toStrict $ Bn.encode b

decode :: Bn.Binary a => BS.ByteString -> a
decode d = Bn.decode $ BSL.fromStrict d

-- Primitive function for receiving a message
receiveMessage :: Bn.Binary a => TCP.Socket -> IO a
receiveMessage socket = do
  -- TODO handle socket closure properly
  Just headerB <- TCP.recv socket 8 -- receive fixed-size, 8 byte header
  let msgLen = decode headerB :: Int -- Bn.decode takes 8 byte ByteStrings and decodes them as Int
  Just bodyB <- TCP.recv socket msgLen
  return $ decode bodyB

-- Primitive function for sending a message
sendMessage :: Bn.Binary a => TCP.Socket -> a -> IO ()
sendMessage socket msg = do
  let bodyB = encode msg
      headerB = encode $ BS.length bodyB
  TCP.send socket headerB
  TCP.send socket bodyB

-- Thread function for receiving
handleReceive :: Bn.Binary a => EndpointId -> C.Chan (EndpointId, a) -> TCP.Socket -> IO ()
handleReceive endpointId chan socket = do
  Mo.forever $ do
    message <- receiveMessage socket
    Lg.debugM Lg.network "Receiving Message"
    C.writeChan chan (endpointId, message)

-- Thread function for sending
handleSend :: Bn.Binary a => C.Chan a -> TCP.Socket -> IO ()
handleSend chan socket = do
  Mo.forever $ do
    msg <- C.readChan chan
    Lg.debugM Lg.network "Sending Message"
    sendMessage socket msg
