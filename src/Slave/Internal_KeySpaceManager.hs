{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Slave.Internal_KeySpaceManager where

import qualified Data.Default as Df
import qualified GHC.Generics as Gn

import qualified Proto.Common as Co
import Infra.Lens

data KeySpaceManager = KeySpaceManager {
  _lat :: Int,
  _versions :: [(Co.Timestamp, Co.RequestId, [Co.KeySpaceRange])]
} deriving (Gn.Generic, Df.Default, Show)

makeLenses ''KeySpaceManager
