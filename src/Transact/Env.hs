{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Transact.Env where

import qualified GHC.Generics as Gn
import qualified System.Random as Rn

import Infra.Lens

data Env = Env {
  _rand :: Rn.StdGen
} deriving (Gn.Generic, Show)

makeLenses ''Env
