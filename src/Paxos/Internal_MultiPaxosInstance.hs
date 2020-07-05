{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Paxos.Internal_MultiPaxosInstance where

import qualified Data.Default as Df
import qualified Data.Map as Mp
import qualified GHC.Generics as Gn

import qualified Paxos.PaxosInstance as PI
import qualified Paxos.PaxosLog as PL
import qualified Proto.Messages.PaxosMessages as PM
import qualified Proto.Common as Co
import Infra.Lens

data MultiPaxosInstance = MultiPaxosInstance {
  _paxosId :: Co.PaxosId,
  _paxosInstances :: Mp.Map PM.IndexT PI.PaxosInstance,
  _paxosLog :: PL.PaxosLog
} deriving (Gn.Generic, Show)

makeLenses ''MultiPaxosInstance

constructor :: Co.PaxosId -> MultiPaxosInstance
constructor paxosId = MultiPaxosInstance {
  _paxosId = paxosId,
  _paxosInstances = Df.def,
  _paxosLog = Df.def
}
