{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Slave.Tablet.TabletState where

import qualified Data.Default as Df
import qualified Data.Map as Mp
import qualified GHC.Generics as Gn

import qualified Proto.Common as Co
import qualified Paxos.MultiPaxosInstance as MP
import qualified Paxos.Tasks.PaxosTaskManager as PTM
import qualified Slave.Tablet.Internal_DerivedState as DS
import qualified Slave.Tablet.Env as En
import Infra.Lens

-- TODO: Evidently, we shouldn't have a default way of constructing this, or at leat it
-- should be private somehow. I've made the mistake of incorrect instantiation twice now. 
data TabletState = TabletState {
  _range :: Co.KeySpaceRange,
  _multiPaxosInstance :: MP.MultiPaxosInstance,
  _derivedState :: DS.DerivedState,
  _paxosTaskManager :: PTM.PaxosTaskManager DS.DerivedState,
  _env :: En.Env
} deriving (Gn.Generic, Df.Default, Show)

makeLenses ''TabletState
