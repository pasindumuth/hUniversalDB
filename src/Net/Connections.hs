{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Net.Connections (
  Connections,
  accept,
  connect,
  selfConnect,
  sendMessage,
  receiveMessage
) where

import qualified Data.Binary as Bn
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Mp
import qualified Control.Concurrent as C
import qualified Control.Concurrent.Chan as Ct
import qualified Control.Concurrent.MVar as MV
import qualified Control.Monad as Mo
import qualified Infra.Utils as U
import qualified Network.Simple.TCP as TCP

import qualified Infra.Logging as Lg
import qualified Proto.Common as Co
import qualified Proto.Messages as Ms

-- The value of Connections is function whose return value is an IO Operator
-- that will add the input message to some Chan somewhere in the Real World.
-- We call these the Output Chans of the Endpoint. There is a Sending Thread
-- that does Ct.readChan from the Output Chans of the Endpoints (of a
-- Connections) and then actually sends the data over the network (it
-- will have the appropriate network socket in its scope).
type Connections = Mp.Map Co.EndpointId (Ms.Message -> IO ())

-- Adds an Output Chan to the Real World, updating Connections accordingly,
-- and returns the Output Chan (for the Sending Thread to start forwarding from).
addConn
  :: MV.MVar Connections
  -> Co.EndpointId
  -> IO (Ct.Chan Ms.Message)
addConn connM endpointId = do
   sendChan <- Ct.newChan
   conn <- MV.takeMVar connM
   MV.putMVar connM (Mp.insert endpointId (Ct.writeChan sendChan) conn)
   return sendChan

delConn
  :: MV.MVar Connections
  -> Co.EndpointId
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
handleReceive
  :: Bn.Binary a
  => Co.EndpointId
  -> Ct.Chan (Co.EndpointId, a)
  -> TCP.Socket
  -> IO ()
handleReceive endpointId chan socket = do
  Mo.forever $ do
    message <- receiveMessage socket
    Lg.debugM Lg.network "Receiving Message"
    Ct.writeChan chan (endpointId, message)

-- Thread function for sending
handleSend
  :: Bn.Binary a
  => Ct.Chan a
  -> TCP.Socket
  -> IO ()
handleSend chan socket = do
  Mo.forever $ do
    msg <- Ct.readChan chan
    Lg.debugM Lg.network "Sending Message"
    sendMessage socket msg

-- This function creates a new Output Channel in the Real World, keyed by
-- the String, and then has the calling thread perpetually reads from this
-- Output Channel and pass it into receiveChan (which is read by the Input
-- Thread and handled by the main server code).
selfConnect
  :: String
  -> MV.MVar Connections
  -> Ct.Chan (Co.EndpointId, Ms.Message)
  -> IO ()
selfConnect ip connM receiveChan = do
  let eId = Co.EndpointId ip
  sendChan <- addConn connM eId
  Lg.infoM Lg.main "Created Self-Connections"
  Mo.forever $ do
    msg <- Ct.readChan sendChan
    Ct.writeChan receiveChan (eId, msg)

createConnHandlers
  :: MV.MVar Connections
  -> Ct.Chan (Co.EndpointId, Ms.Message)
  -> (TCP.Socket, TCP.SockAddr)
  -> IO ()
createConnHandlers connM receiveChan (socket, remoteAddr) = do
  let eId  = Co.EndpointId $ U.prefix  ':' $ show remoteAddr
  Lg.infoM Lg.main $ "Connection established to " ++ (Co.getEndpointId eId)
  sendChan <- addConn connM eId
  C.forkFinally (handleSend sendChan socket) $ \_ -> delConn connM eId
  handleReceive eId receiveChan socket

-- Listens to connections at port 8000. When a connection comes in,
-- this function adds a new Output Chan to connM, creates a Sending
-- Thread for it to send messages that're passed into the Output Chan,
-- and creates a Receiving Thread that reads from the
-- socket and passes the data into the receiveChan, the Input Channel.
accept
  :: MV.MVar Connections
  -> Ct.Chan (Co.EndpointId, Ms.Message)
  -> IO ()
accept connM receiveChan =
  TCP.serve TCP.HostAny "8000" $ createConnHandlers connM receiveChan

-- Starts to connect to the given ip from port 8000. When a connection
-- comes in, this function adds a new Output Chan to connM, creates a
-- Sending Thread for it to send messages that're passed into the Output
-- Chan, and creates a Receiving Thread that reads from the socket
-- and passes the data into the receiveChan, the Input Channel.
connect
  :: String
  -> MV.MVar Connections
  -> Ct.Chan (Co.EndpointId, Ms.Message)
  -> IO ()
connect ip connM receiveChan =
  TCP.connect ip "8000" $ createConnHandlers connM receiveChan
