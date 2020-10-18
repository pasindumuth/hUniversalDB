{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Transact.Server.ServerState where

import qualified GHC.Generics as Gn
import qualified System.Random as Rn

import qualified Transact.Server.Env as En
import Infra.Lens

data ServerState = ServerState {
  _env :: En.Env
} deriving (Gn.Generic, Show)

makeLenses ''ServerState

constructor :: Rn.StdGen -> ServerState
constructor rand = ServerState {
  _env = En.Env rand
}
