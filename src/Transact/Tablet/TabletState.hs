{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Transact.Tablet.TabletState where

import qualified GHC.Generics as Gn
import qualified System.Random as Rn

import qualified Transact.Tablet.Env as En
import Infra.Lens

data TabletState = TabletState {
  _env :: En.Env
} deriving (Gn.Generic, Show)

makeLenses ''TabletState

constructor :: Rn.StdGen -> TabletState
constructor rand = TabletState {
  _env = En.Env rand
}
