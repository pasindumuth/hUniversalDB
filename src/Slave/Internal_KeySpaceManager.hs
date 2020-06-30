{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Slave.Internal_KeySpaceManager where

import qualified Data.Default as Df
import qualified GHC.Generics as Gn

import qualified Proto.Common as Co

data KeySpaceManager = KeySpaceManager {
  ranges :: [Co.KeySpaceRange],
  generation :: Int
} deriving (Gn.Generic, Df.Default, Show)
