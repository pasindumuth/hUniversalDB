{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Proto.Common where

import qualified Data.Binary as Bn
import qualified Data.Default as Df
import qualified GHC.Generics as Gn

type EndpointId = String
type PaxosId = String

type RequestId = String
type DatabaseId = String
type TableId = String

type ErrorMsg = String

data KeySpaceRange = KeySpaceRange {
  databaseId :: String,
  tableId :: String,
  keyStart :: Maybe String,
  keyEnd :: Maybe String
} deriving (Gn.Generic, Df.Default, Bn.Binary, Show, Eq, Ord)
