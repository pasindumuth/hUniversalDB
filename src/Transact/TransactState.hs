{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Transact.TransactState where

import qualified GHC.Generics as Gn
import qualified System.Random as Rn

import qualified Transact.Env as En
import Infra.Lens

data TransactState = TransactState {
  _env :: En.Env
} deriving (Gn.Generic, Show)

makeLenses ''TransactState

constructor :: Rn.StdGen -> TransactState
constructor rand = TransactState {
  _env = En.Env rand
}
