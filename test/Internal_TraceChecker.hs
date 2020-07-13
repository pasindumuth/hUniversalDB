{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Internal_TraceChecker where

import qualified Data.Default as Df
import qualified Data.Map as Mp
import qualified GHC.Generics as Gn

import qualified Proto.Common as Co
import qualified Proto.Messages.ClientRequests as CRq
import qualified Proto.Messages.PaxosMessages as PM
import qualified Slave.Tablet.MultiVersionKVStore as MVS
import qualified Slave.KeySpaceManager as KSM
import Infra.Lens

data CheckState = CheckState {
  _requestMap :: Mp.Map Co.RequestId CRq.Payload,
  _rangeMap :: Mp.Map Co.PaxosId Co.KeySpaceRange,
  _tables :: Mp.Map Co.KeySpaceRange MVS.MultiVersionKVStore,
  _keySpaceManager :: KSM.KeySpaceManager
} deriving (Gn.Generic, Df.Default)

makeLenses ''CheckState

data TestPaxosLog = TestPaxosLog {
  _plog :: Mp.Map PM.IndexT PM.PaxosLogEntry,
  _nextIdx :: PM.IndexT
}

makeLenses ''TestPaxosLog
