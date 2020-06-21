{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Records.MultiPaxosInstance where

import qualified Data.Default as D
import qualified Data.Map as Mp
import qualified GHC.Generics as G

import qualified PaxosInstance as PI
import qualified PaxosLog as PL
import qualified Message as M
import Lens (makeLenses)

data MultiPaxosInstance = MultiPaxosInstance {
  _paxosLog :: PL.PaxosLog,
  _paxosInstances :: Mp.Map M.IndexT PI.PaxosInstance
} deriving (G.Generic, D.Default, Show)

makeLenses ''MultiPaxosInstance
