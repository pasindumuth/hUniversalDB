{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Master.SlaveGroupRanges (
  SlaveGroupRanges
) where

import qualified Data.Default as Df
import qualified Data.Map as Mp
import qualified GHC.Generics as Gn

import qualified Proto.Common as Co
import Infra.Lens

data Value = Value Co.KeySpace Co.NewKeySpace deriving (Show)

data SlaveGroupRanges = SlaveGroupRanges {
  _i'lat :: Co.Lat,
  _i'ranges :: Mp.Map Co.SlaveGroupId [(Co.Timestamp, Value)]
} deriving (Gn.Generic, Df.Default, Show)
