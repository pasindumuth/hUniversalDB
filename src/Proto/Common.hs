{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Proto.Common where

import qualified Data.Default as Df
import qualified GHC.Generics as Gn

type EndpointId = String

data KeySpaceRange = KeySpaceRange {
  databaseId :: String,
  tableId :: String,
  keyStart :: String,
  keyEnd :: String
} deriving (Gn.Generic, Df.Default, Show)
