{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Common.Model.RelationalTablet where

import qualified Data.Binary as Bn
import qualified GHC.Generics as Gn

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

newtype PrimaryKey = PrimaryKey [ColumnValue] deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)
newtype Row = Row [ColumnValue] deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)
