{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module IncomingMessageHandler where

import qualified Data.Default as D
import qualified Data.Map as Mp
import qualified Data.Maybe as Mb
import qualified Data.Sequence as Sq
import qualified Control.Concurrent as C
import qualified Control.Concurrent.MVar as MV
import qualified Control.Concurrent.Chan as Ch
import qualified GHC.Generics as G
import qualified System.Random as R

import qualified Connections as CC
import qualified MultiPaxosInstance as MP
import qualified PaxosLog as PL
import qualified MultiVersionKVStore as MS
import qualified ClientRequestHandler as CR
import qualified Message as M
import qualified TabletParticipant as TP
import qualified Utils as U
import Lens (makeLenses, (%~), (.~), (^.), (&), (?~), at, ix, (.^.), _1, _2, wrapMaybe, lensProduct)

data IMHMessage =
  ClientRequest CC.EndpointId M.ClientRequest |
  SlaveMessage CC.EndpointId M.MultiPaxosMessage |
  Retry M.Retry
  deriving (Show)

incomingMessageHandlerIO :: Ch.Chan IMHMessage -> IO ()
incomingMessageHandlerIO channel = do
  imhRepeat D.def
  where
    imhRepeat :: CR.GlobalState -> IO ()
    imhRepeat state = do
      nextMessage <- Ch.readChan channel
      case nextMessage of
        ClientRequest eId r -> do
          let (retryM, state') = CR.clientRequestHandler eId r state
          case retryM of
            Nothing -> imhRepeat state
            Just retry -> do
              C.forkIO $ do
                C.threadDelay 100000
                Ch.writeChan channel $ Retry retry
              imhRepeat state'

