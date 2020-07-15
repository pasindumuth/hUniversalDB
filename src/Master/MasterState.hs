{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Master.MasterState where

import qualified Data.Default as Df
import qualified Data.Map as Mp
import qualified GHC.Generics as Gn
import qualified System.Random as Rn

import qualified Paxos.MultiPaxosInstance as MP
import qualified Paxos.Tasks.PaxosTaskManager as PTM
import qualified Proto.Actions.MasterActions as MAc
import qualified Proto.Common as Co
import qualified Master.DerivedState as DS
import qualified Master.Env as En
import Infra.Lens

data MasterState = MasterState {
  _multiPaxosInstance :: MP.MultiPaxosInstance,
  _derivedState :: DS.DerivedState,
  _paxosTaskManager :: PTM.PaxosTaskManager DS.DerivedState MAc.OutputAction,
  _env :: En.Env
} deriving (Gn.Generic, Show)

makeLenses ''MasterState

constructor
  :: Co.PaxosId
  -> Rn.StdGen
  -> [Co.EndpointId]
  -> [Co.EndpointId]
  -> MasterState
constructor paxosId rand masterEIds slaveEIds = MasterState {
  _multiPaxosInstance = MP.constructor paxosId,
  _derivedState = DS.constructor slaveEIds,
  _paxosTaskManager = Df.def,
  _env = En.Env rand masterEIds
}
