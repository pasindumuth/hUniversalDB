{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Slave.Tablet.Internal_DerivedState where

import qualified Data.Default as Df
import qualified GHC.Generics as Gn

import qualified Slave.Tablet.Internal_MultiVersionKVStore as MS
import Infra.Lens

data DerivedState = DerivedState {
  _kvStore :: MS.MultiVersionKVStore
} deriving (Gn.Generic, Df.Default, Show)

makeLenses ''DerivedState