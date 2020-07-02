{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Slave.SlaveState where

import qualified Data.Default as Df
import qualified Data.Map as Mp
import qualified GHC.Generics as Gn

import qualified Paxos.MultiPaxosInstance as MP
import qualified Paxos.Tasks.PaxosTaskManager as PTM
import qualified Slave.Internal_DerivedState as DS
import qualified Slave.Env as En
import Infra.Lens

data SlaveState = SlaveState {
  _multiPaxosInstance :: MP.MultiPaxosInstance,
  _derivedState :: DS.DerivedState,
  _paxosTaskManager :: PTM.PaxosTaskManager DS.DerivedState,
  _env :: En.Env
} deriving (Gn.Generic, Df.Default, Show)

makeLenses ''SlaveState
