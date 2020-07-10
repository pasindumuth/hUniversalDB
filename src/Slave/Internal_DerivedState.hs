{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Slave.Internal_DerivedState where

import qualified Data.Default as Df
import qualified GHC.Generics as Gn

import qualified Slave.KeySpaceManager as KSM
import Infra.Lens

data DerivedState = DerivedState {
  _keySpaceManager :: KSM.KeySpaceManager
} deriving (Gn.Generic, Df.Default, Show)

makeLenses ''DerivedState
