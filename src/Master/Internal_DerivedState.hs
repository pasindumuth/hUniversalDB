{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Master.Internal_DerivedState where

import qualified Data.Default as Df
import qualified Data.Map as Mp
import qualified GHC.Generics as Gn

import qualified Proto.Common as Co
import qualified Master.Internal_SlaveGroupRanges as SGR
import qualified Master.Internal_NetworkTask as NT
import Infra.Lens

data DerivedState = DerivedState {
  _slaveGroupRanges :: SGR.SlaveGroupRanges,
  _taskMap :: Mp.Map Co.UID NT.NetworkTask
} deriving (Gn.Generic, Df.Default, Show)

makeLenses ''DerivedState
