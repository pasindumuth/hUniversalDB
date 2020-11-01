{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Transact.Model.Common (
  module Transact.Model.Common,
  Co.EndpointId (Co.EndpointId),
  Co.RequestId (Co.RequestId)
) where

import qualified Data.Binary as Bn
import qualified Data.ByteString as BS
import qualified Data.Default as Df
import qualified GHC.Generics as Gn

import qualified Common.Model.RelationalTablet as RT
import qualified Proto.Common as Co

import Infra.Lens

-- This globally identifies a table of a database in a whole deployment of hUniversalDB.
-- Note that this means that TabletPath encompasses both the table name and the database name.
newtype TabletPath = TabletPath { getPath :: String } deriving (Gn.Generic, Df.Default, Bn.Binary, Show, Eq, Ord)

-- This represents a subset of keys: [startKey, endKey). If startKey/endKey
-- are Nothing, there is no lower/upper bound.
data TabletKeyRange = TabletKeyRange {
  _startKey :: Maybe RT.PrimaryKey,
  _endKey :: Maybe RT.PrimaryKey
} deriving (Gn.Generic, Df.Default, Bn.Binary, Show, Eq, Ord)

-- This globally identifies a tablet of a table of a database at a given
-- moment in time in a whole deployment of hUniversalDB. Recall that a
-- deployment is a database cluster.
data TabletShape = TabletShape {
  _path :: TabletPath,
  _range :: TabletKeyRange
} deriving (Gn.Generic, Df.Default, Bn.Binary, Show, Eq, Ord)
