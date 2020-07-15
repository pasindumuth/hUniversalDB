{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Slave.Tablet.DerivedState where

import qualified Data.Default as Df
import qualified GHC.Generics as Gn

import qualified Slave.Tablet.MultiVersionKVStore as MVS
import Infra.Lens

data DerivedState = DerivedState {
  _kvStore :: MVS.MultiVersionKVStore
} deriving (Gn.Generic, Df.Default, Show)

makeLenses ''DerivedState
