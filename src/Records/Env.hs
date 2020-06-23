{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Records.Env where

import qualified Data.Default as D
import qualified GHC.Generics as G

import qualified System.Random as R
import qualified Records.Common.Common as C
import Lens (makeLenses)

data Env = Env {
  _rand :: R.StdGen,
  _slaveEIds :: [C.EndpointId]
} deriving (G.Generic, Show)

instance D.Default Env where
  def = Env (R.mkStdGen 0) []

makeLenses ''Env
