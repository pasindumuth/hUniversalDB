{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Records.DerivedState where

import qualified Data.Default as D
import qualified GHC.Generics as G

import qualified Records.MultiVersionKVStore as MS
import Lens (makeLenses)

data DerivedState = DerivedState {
  _kvStore :: MS.MultiVersionKVStore
} deriving (G.Generic, D.Default, Show)

makeLenses ''DerivedState
