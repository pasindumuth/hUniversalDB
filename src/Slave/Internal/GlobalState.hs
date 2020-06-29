{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Slave.Internal.GlobalState where

import qualified Data.Default as Df
import qualified Data.Map as Mp
import qualified GHC.Generics as Gn

import qualified Paxos.Internal.MultiPaxosInstance as MP
import qualified Paxos.Internal.PaxosLog as PL
import qualified Slave.Internal.SlaveRequestManager as SRM
import qualified Slave.Internal.DerivedState as DS
import qualified Slave.Internal.Env as En
import Infra.Lens

data GlobalState = GlobalState {
  _paxosLog :: PL.PaxosLog,
  _multiPaxosInstance :: MP.MultiPaxosInstance,
  _derivedState :: DS.DerivedState,
  _slaveRequestManager :: SRM.SlaveRequestManager,
  _env :: En.Env
} deriving (Gn.Generic, Df.Default, Show)

makeLenses ''GlobalState
