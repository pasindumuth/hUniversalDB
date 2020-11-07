{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Common.Model.RelationalTablet where

import qualified Data.Binary as Bn
import qualified GHC.Generics as Gn
import Infra.Lens

------------------------------------------------------------------------------------------------------------------------
-- Column Description Types
------------------------------------------------------------------------------------------------------------------------
data ColumnType
  = CT'String
  | CT'Int
  | CT'Double
  | CT'Bool
  | CT'Empty
  deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)

data ColumnValue
  = CV'String String
  | CV'Int Int
  | CV'Double Double
  | CV'Bool Bool
  | CV'Empty ()
  deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)

------------------------------------------------------------------------------------------------------------------------
-- General Tablet Types
------------------------------------------------------------------------------------------------------------------------

data Schema = Schema {
  _schemaKey :: [(String, ColumnType)],
  _schemaColumns :: [(String, ColumnType)]
} deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)

makeLenses ''Schema

newtype PrimaryKey = PrimaryKey {
  getPrimaryKey :: [ColumnValue]
} deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)

data Row = Row {
  _rowKey :: PrimaryKey,
  _rowColumns :: [Maybe ColumnValue]
} deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)

makeLenses ''Row
