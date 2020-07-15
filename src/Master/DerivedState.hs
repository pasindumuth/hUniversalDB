{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Master.DerivedState where

import qualified GHC.Generics as Gn

import qualified Master.SlaveGroupRanges as SGR
import qualified Master.NetworkTaskManager as NTM
import qualified Proto.Common as Co
import Infra.Lens

data DerivedState = DerivedState {
  _slaveGroupRanges :: SGR.SlaveGroupRanges,
  _networkTaskManager :: NTM.NetworkTaskManager,
  _slaveEIds :: [Co.EndpointId]
} deriving (Gn.Generic, Show)

makeLenses ''DerivedState
