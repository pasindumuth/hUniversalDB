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
  _shapesWithSchema :: [(RT.Schema, Co.TabletShape)],
  _env :: En.Env
} deriving (Gn.Generic, Show)

makeLenses ''ServerState

constructor
  :: [(RT.Schema, Co.TabletShape)]
  -> Rn.StdGen
  -> ServerState
constructor shapesWithSchema rand = ServerState {
  _shapesWithSchema = shapesWithSchema,
  _env = En.Env rand
}
