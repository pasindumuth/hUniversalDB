{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Slave.Tablet.Internal.GlobalState where

import qualified Data.Default as Df
import qualified Data.Map as Mp
import qualified GHC.Generics as Gn

import qualified Paxos.Internal.MultiPaxosInstance as MP
import qualified Paxos.Internal.PaxosLog as PL
import qualified Paxos.Tasks.PaxosTaskManager as PTM
import qualified Slave.Tablet.Internal.DerivedState as DS
import qualified Slave.Tablet.Internal.Env as En
import Infra.Lens

data GlobalState = GlobalState {
  _paxosLog :: PL.PaxosLog,
  _multiPaxosInstance :: MP.MultiPaxosInstance,
  _derivedState :: DS.DerivedState,
  _paxosTaskManager :: PTM.PaxosTaskManager DS.DerivedState,
  _env :: En.Env
} deriving (Gn.Generic, Df.Default, Show)

makeLenses ''GlobalState
