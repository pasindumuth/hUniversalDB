{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Master.Env where

import qualified GHC.Generics as Gn
import qualified System.Random as Rn

import qualified Proto.Common as Co
import Infra.Lens

data Env = Env {
  _rand :: Rn.StdGen,
  _masterEIds :: [Co.EndpointId]
} deriving (Gn.Generic, Show)

makeLenses ''Env
