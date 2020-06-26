{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Records.MultiPaxosInstance where

import qualified Data.Default as Df
import qualified Data.Map as Mp
import qualified GHC.Generics as Gn

import qualified PaxosInstance as PI
import qualified Records.Messages.PaxosMessages as PM
import Lens

data MultiPaxosInstance = MultiPaxosInstance {
  _paxosInstances :: Mp.Map PM.IndexT PI.PaxosInstance
} deriving (Gn.Generic, Df.Default, Show)

makeLenses ''MultiPaxosInstance
