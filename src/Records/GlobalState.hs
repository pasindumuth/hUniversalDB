{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Records.GlobalState where

import qualified Data.Default as Df
import qualified Data.Map as Mp
import qualified GHC.Generics as Gn

import qualified Paxos.Internal.MultiPaxosInstance as MP
import qualified Paxos.Internal.PaxosLog as PL
import qualified Records.DerivedState as DS
import qualified Records.ClientRequestManager as CRM
import qualified Records.Env as En
import Lens

data GlobalState = GlobalState {
  _paxosLog :: PL.PaxosLog,
  _multiPaxosInstance :: MP.MultiPaxosInstance,
  _derivedState :: DS.DerivedState,
  _clientRequestManager :: CRM.ClientRequestManager,
  _env :: En.Env
} deriving (Gn.Generic, Df.Default, Show)

makeLenses ''GlobalState
