{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Records.GlobalState where

import qualified Data.Default as D
import qualified Data.Map as Mp
import qualified GHC.Generics as G

import qualified Records.MultiPaxosInstance as MP
import qualified Records.PaxosLog as PL
import qualified Records.DerivedState as DS
import qualified Records.ClientRequestManager as CRM
import qualified Records.Env as E
import Lens

data GlobalState = GlobalState {
  _paxosLog :: PL.PaxosLog,
  _multiPaxosInstance :: MP.MultiPaxosInstance,
  _derivedState :: DS.DerivedState,
  _clientRequestManager :: CRM.ClientRequestManager,
  _env :: E.Env
} deriving (G.Generic, D.Default, Show)

makeLenses ''GlobalState
