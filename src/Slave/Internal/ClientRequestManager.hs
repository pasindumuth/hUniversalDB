{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Slave.Internal.ClientRequestManager where

import qualified Data.Default as Df
import qualified Data.Sequence as Sq
import qualified GHC.Generics as Gn

import qualified Proto.Common as Co
import qualified Slave.Internal.MultiVersionKVStore as MS
import qualified Proto.Messages.PaxosMessages as PM
import qualified Proto.Messages.ClientMessages as CM
import Infra.Lens

data CurrentInsert = CurrentInsert {
  _index :: Int,
  _entry :: PM.PaxosLogEntry,
  _retryCount :: Int,
  _clientMessage :: CM.ClientRequest,
  _eId :: Co.EndpointId,
  _counterValue :: Int
} deriving (Gn.Generic, Show)

data ClientRequestManager = ClientRequestManager {
  _currentInsert :: Maybe CurrentInsert,
  _requestQueue :: Sq.Seq (Co.EndpointId, CM.ClientRequest),
  _counter :: Int
} deriving (Gn.Generic, Df.Default, Show)

makeLenses ''CurrentInsert
makeLenses ''ClientRequestManager
