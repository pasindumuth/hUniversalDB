{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Transact.Server.ServerState where

import qualified GHC.Generics as Gn
import qualified System.Random as Rn

import qualified Common.Model.RelationalTablet as RT
import qualified Transact.Model.Common as Co
import qualified Transact.Server.Env as En
import Infra.Lens

data ServerState = ServerState {
  _env :: En.Env,
  _shapesWithSchema :: [(RT.Schema, Co.TabletShape)]
} deriving (Gn.Generic, Show)

makeLenses ''ServerState

constructor
  :: Rn.StdGen
  -> [(RT.Schema, Co.TabletShape)]
  -> ServerState
constructor rand shapesWithSchema = ServerState {
  _env = En.Env rand,
  _shapesWithSchema = shapesWithSchema
}
