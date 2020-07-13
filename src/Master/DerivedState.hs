{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Master.DerivedState (
  DerivedState
) where

import qualified Data.Default as Df
import qualified Data.Map as Mp
import qualified GHC.Generics as Gn

import qualified Proto.Common as Co
import qualified Master.SlaveGroupRanges as SGR
import qualified Master.NetworkTask as NT
import Infra.Lens

data DerivedState = DerivedState {
  _i'slaveGroupRanges :: SGR.SlaveGroupRanges,
  _i'taskMap :: Mp.Map Co.UID NT.NetworkTask
} deriving (Gn.Generic, Df.Default, Show)

makeLenses ''DerivedState
