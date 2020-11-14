{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Transact.Model.SqlAST where

import qualified Data.Binary as Bn
import qualified GHC.Generics as Gn

------------------------------------------------------------------------------------------------------------------------
-- Values
------------------------------------------------------------------------------------------------------------------------
data CompareOp
  = OpGT -- The > operator
  | OpGTE -- The >= operator
  | OpE -- The = operator
  | OpLTE -- The <= operator
  | OpLT -- The < operator
  deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)

------------------------------------------------------------------------------------------------------------------------
-- Abstract Syntax Tree
------------------------------------------------------------------------------------------------------------------------
data SqlStatement
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
  = WhereBody BoolDisjunction
  deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)

-- | These are bool expression stringed together by OR. For
-- example, `... OR ... OR ... `.
data BoolDisjunction
  = BoolDisjunction [BoolConjunction]
  deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)

-- | These are bool expression stringed together by AND. For
-- example, `... AND ... AND ... `.
data BoolConjunction
  = BoolConjunction [BoolExprAtom]
  deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)

-- | These are what's containied in on either side of an AND.
data BoolExprAtom
  = SubBoolDisjunction BoolDisjunction
  -- | An expression with a comparison operator, we there is a value
  -- or expression on the left side and the right side.
  | CompareExpr ValueAtom CompareOp ValueAtom
  | SingleBoolValue Bool
  deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)

data ValueAtom
  = StringValue String
  | BoolValue Bool
  | IntValue Int
  | FloatValue Float
  -- | This holds either the column name of a table or an alias given to
  -- it in the SELECT body. Note that the parser doesn't check to see
  -- if the alias exists.
  | ColumnName String -- This is just a column name
  | QualifiedColumnName String String -- This is something like `alias.column_name`
  deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)
