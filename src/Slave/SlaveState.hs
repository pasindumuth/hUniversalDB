{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Slave.SlaveState where

import qualified Data.Default as Df
import qualified Data.Map as Mp
import qualified GHC.Generics as Gn
import qualified System.Random as Rn

import qualified Paxos.MultiPaxosInstance as MP
import qualified Paxos.Tasks.PaxosTaskManager as PTM
import qualified Proto.Actions.SlaveActions as SAc
import qualified Proto.Common as Co
import qualified Proto.Messages.TraceMessages as TrM
import qualified Slave.DerivedState as DS
import qualified Slave.Env as En
import Infra.Lens

data SlaveState = SlaveState {
  _slaveGroupId :: Co.SlaveGroupId,
  _multiPaxosInstance :: MP.MultiPaxosInstance,
  _derivedState :: DS.DerivedState,
  _paxosTaskManager :: PTM.PaxosTaskManager DS.DerivedState SAc.OutputAction TrM.TraceMessage,
  _env :: En.Env
} deriving (Gn.Generic, Show)

makeLenses ''SlaveState

constructor
  :: Co.SlaveGroupId -- Right now, this is only used for testing
  -> Co.PaxosId -- Right now, this is only used for testing
  -> Rn.StdGen
  -> [Co.EndpointId]
  -> SlaveState
constructor slaveGroupId paxosId rand slaveEIds = SlaveState {
  _slaveGroupId = slaveGroupId, 
  _multiPaxosInstance = MP.constructor paxosId,
  _derivedState = Df.def,
  _paxosTaskManager = Df.def,
  _env = En.Env rand slaveEIds
}
