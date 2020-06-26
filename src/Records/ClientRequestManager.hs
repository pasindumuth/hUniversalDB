{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Records.ClientRequestManager where

import qualified Data.Default as D
import qualified Data.Sequence as Sq
import qualified GHC.Generics as G

import qualified Records.Common.Common as C
import qualified Records.MultiVersionKVStore as MS
import qualified Records.Messages.PaxosMessages as PM
import qualified Records.Messages.ClientMessages as CM
import Lens (makeLenses)

data CurrentInsert = CurrentInsert {
  _index :: Int,
  _entry :: PM.PaxosLogEntry,
  _retryCount :: Int,
  _clientMessage :: CM.ClientRequest,
  _eId :: C.EndpointId,
  _counterValue :: Int
} deriving (G.Generic, Show)

data ClientRequestManager = ClientRequestManager {
  _currentInsert :: Maybe CurrentInsert,
  _requestQueue :: Sq.Seq (C.EndpointId, CM.ClientRequest),
  _counter :: Int
} deriving (G.Generic, D.Default, Show)

makeLenses ''CurrentInsert
makeLenses ''ClientRequestManager
