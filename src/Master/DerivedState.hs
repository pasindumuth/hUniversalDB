{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Master.DerivedState where

import qualified Data.Default as Df
import qualified Data.Map as Mp
import qualified GHC.Generics as Gn

import qualified Master.SlaveGroupRanges as SGR
import qualified Master.NetworkTaskManager as NTM
import qualified Proto.Common as Co
import Infra.Lens

data DerivedState = DerivedState {
  _slaveGroupRanges :: SGR.SlaveGroupRanges,
  _networkTaskManager :: NTM.NetworkTaskManager,
  _slaveGroupEIds :: Mp.Map Co.SlaveGroupId [Co.EndpointId]
} deriving (Gn.Generic, Show)

makeLenses ''DerivedState

constructor
  :: Mp.Map Co.SlaveGroupId [Co.EndpointId]
  -> DerivedState
constructor slaveGroupEIds = DerivedState {
  _slaveGroupRanges = SGR.constructor $ Mp.keys slaveGroupEIds,
  _networkTaskManager = Df.def,
  _slaveGroupEIds = slaveGroupEIds
}
