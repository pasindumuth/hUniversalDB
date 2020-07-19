{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Slave.Tablet.TabletState where

import qualified Data.Default as Df
import qualified Data.Map as Mp
import qualified GHC.Generics as Gn
import qualified System.Random as Rn

import qualified Proto.Common as Co
import qualified Paxos.MultiPaxosInstance as MP
import qualified Paxos.Tasks.PaxosTaskManager as PTM
import qualified Proto.Actions.TabletActions as TAc
import qualified Proto.Common as Co
import qualified Slave.Tablet.DerivedState as DS
import qualified Slave.Tablet.Env as En
import Infra.Lens

data TabletState = TabletState {
  _tabletId :: Co.TabletId,
  _multiPaxosInstance :: MP.MultiPaxosInstance,
  _derivedState :: DS.DerivedState,
  _paxosTaskManager :: PTM.PaxosTaskManager DS.DerivedState TAc.OutputAction,
  _env :: En.Env
} deriving (Gn.Generic, Show)

makeLenses ''TabletState

constructor
  :: Co.TabletId
  -> Rn.StdGen
  -> [Co.EndpointId]
  -> TabletState
constructor tabletId rand slaveEIds = TabletState {
  _tabletId = tabletId,
  _multiPaxosInstance = MP.constructor (Co.getPaxosId tabletId),
  _derivedState = Df.def,
  _paxosTaskManager = Df.def,
  _env = En.Env rand slaveEIds
}
