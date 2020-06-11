module Network where

import qualified Control.Monad as Mo
import qualified Data.Binary as B
import qualified Data.Binary.Put as BP
import qualified Data.Binary.Get as BG
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Default as D
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Control.Concurrent.Chan as C
import qualified Control.Concurrent.MVar as MV
import qualified Control.Monad.State as St
import qualified Network.Simple.TCP as TCP
import qualified System.Environment as E

type EndpointId = String

-- Primitive function for receiving a message
receiveMessage :: B.Binary a => TCP.Socket -> IO a
receiveMessage socket = do
  -- TODO handle socket closure properly
  Just headerB <- TCP.recv socket 8 -- receive fixed-size, 8 byte header
  let msgLen = B.decode (BSL.fromStrict headerB) :: Int -- B.decode takes 8 byte ByteStrings and decodes them as Int
  Just bodyB <- TCP.recv socket msgLen
  return (B.decode (BSL.fromStrict bodyB))

-- Primitive function for sending a message
sendMessage :: B.Binary a => TCP.Socket -> a -> IO ()
sendMessage socket msg = do
  let bodyB = BSL.toStrict (BP.runPut (B.put msg))
      headerB = BSL.toStrict (B.encode (BS.length bodyB))
  TCP.send socket headerB
  TCP.send socket bodyB

-- Thread function for receiving
handleReceive :: B.Binary a => EndpointId -> C.Chan (EndpointId, a) -> TCP.Socket -> IO ()
handleReceive endpointId chan socket = do
  Mo.forever $ do
    message <- receiveMessage socket
    C.writeChan chan (endpointId, message)

-- Thread function for sending
handleSend :: B.Binary a => C.Chan a -> TCP.Socket -> IO ()
handleSend chan socket = do
  Mo.forever $ do
    msg <- C.readChan chan
    sendMessage socket msg
