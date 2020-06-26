{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Records.Env where

import qualified Data.Default as Df
import qualified GHC.Generics as Gn

import qualified System.Random as Rn
import qualified Records.Common.Common as Co
import Lens

data Env = Env {
  _rand :: Rn.StdGen,
  _slaveEIds :: [Co.EndpointId]
} deriving (Gn.Generic, Show)

instance Df.Default Env where
  def = Env (Rn.mkStdGen 0) []

makeLenses ''Env
