{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Records.DerivedState where

import qualified Data.Default as Df
import qualified GHC.Generics as Gn

import qualified Records.MultiVersionKVStore as MS
import Lens

data DerivedState = DerivedState {
  _kvStore :: MS.MultiVersionKVStore
} deriving (Gn.Generic, Df.Default, Show)

makeLenses ''DerivedState
