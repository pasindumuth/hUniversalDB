{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Transact.Model.SqlAST where

import qualified Data.Binary as Bn
import qualified GHC.Generics as Gn

data SelectStatement
  = SelectStatement SelectBody FromBody WhereBody
  deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)

data SelectBody
  = SelectBody [ColumnNameAlias]
  deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)

data ColumnNameAlias
  = ColumnNameAlias String (Maybe String)
  deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)

data FromBody
  = FromBody TableNameAlias
  deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)

data TableNameAlias
  = TableNameAlias String (Maybe String)
  deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)

data WhereBody
  = WhereBody Bool
  deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)
