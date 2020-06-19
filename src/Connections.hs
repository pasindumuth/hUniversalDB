{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Connections where

import qualified Data.Map as Mp
import qualified Control.Concurrent.Chan as C
import qualified Control.Concurrent.MVar as MV

import qualified Message as M

type EndpointId = String
type Connections = Mp.Map EndpointId (M.Message -> IO ())

-- remove from common code; only EndpointId should be common

addConn
  :: MV.MVar Connections
  -> EndpointId
  -> IO (C.Chan M.Message)
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
