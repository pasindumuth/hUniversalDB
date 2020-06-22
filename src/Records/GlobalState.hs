{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Records.GlobalState where

import qualified Data.Default as D
import qualified Data.Map as Mp
import qualified GHC.Generics as G

import qualified Records.MultiPaxosInstance as MP
import qualified PaxosLog as PL
import Lens (makeLenses)

data GlobalState = GlobalState {
  _paxosLog :: PL.PaxosLog,
  _multiPaxosInstance :: MP.MultiPaxosInstance
} deriving (G.Generic, D.Default, Show)

makeLenses ''GlobalState
