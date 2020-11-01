{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Transact.Tablet.TabletState where

import qualified Data.Map as Mp
import qualified GHC.Generics as Gn
import qualified System.Random as Rn

import qualified Common.Model.RelationalTablet as RTT
import qualified Common.RelationalTablet as RT
import qualified Transact.Tablet.Env as En
import Infra.Lens

data TabletState = TabletState {
  _env :: En.Env,
  _relationalTablet :: RT.RelationalTablet
} deriving (Gn.Generic, Show)

makeLenses ''TabletState

constructor :: Rn.StdGen -> RTT.Schema -> TabletState
constructor rand schema = TabletState {
  _env = En.Env rand,
  _relationalTablet = RT.RelationalTablet schema Mp.empty
}
