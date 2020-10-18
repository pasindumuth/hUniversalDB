{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Common.RelationalTablet where

import qualified Data.Binary as Bn
import qualified Data.Default as Df
import qualified GHC.Generics as Gn

import qualified Common.MultiVersionMap as MVM
import qualified Proto.Common as Co
import Infra.Lens

-- A RelationalTablet in the abstract best conceptualized as the type definitions
-- RelationalTablet and TabletStorage indicate. The storage is an MVMap,
-- where the key space is the Cartesian product of primary-key columns and
-- Maybe String, where String is the set of column names of the table. The only
-- values possible for keys of the form (_, Nothing) is Just CV'Empty and Nothing.
-- For (_, Just columnName), the values are that of the columnName's ColumnType,
-- and Nothing.
--
-- The key-value pair where the key is (_, Nothing) indicate whether the row
-- exists or not (i.e. if the value is Just CV'Empty, it exists, and if it's Nothing,
-- it doesn't). If (k, Nothing) is Nothing at timestamp t, then all (k, Just _)
-- should also be Nothing at timestamp t. Otherwise, (k, Just _) can be non-Nothing,
-- but if it is Nothing, it indicates the value of the column is NULL (in
-- SQL terms).

--------------------------------------------------------------------------
-- Column Description Types
--------------------------------------------------------------------------
data ColumnType =
  CT'String |
  CT'Int |
  CT'Double |
  CT'Bool |
  CT'Empty
  deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)

data ColumnValue =
  CV'String String |
  CV'Int Int |
  CV'Double Double |
  CV'Bool Bool |
  CV'Empty ()
  deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)

--------------------------------------------------------------------------
-- Relational Tablet
--------------------------------------------------------------------------
type TabletStorage = MVM.MultiVersionMap ([ColumnValue], Maybe String) ColumnValue

-- In this schema definition, the bool at the end indicates if the
-- if the column is a primary key column.
data RelationalTablet = RelationalTablet {
  _i'schema :: [(String, ColumnType, Bool)],
  _i'storage :: TabletStorage
}

makeLenses ''RelationalTablet

-- Checks is the ColumnValue has the type that's indicated by the ColumnType.
checkTypeMatch :: ColumnType -> ColumnValue -> Bool
checkTypeMatch columnType columnValue =
  case (columnType, columnValue) of
    (CT'String, CV'String _) -> True
    (CT'Int, CV'Int _) -> True
    (CT'Double, CV'Double _) -> True
    (CT'Bool, CV'Bool _) -> True
    (CT'Empty, CV'Empty _) -> True
    _ -> False

-- Inserts the row into the RelationalTablet. This method assumes that this is
-- a valid operation; the timestamp is greater than the timestamps of all column
-- Lats for the given primary key.
insertRow
  :: RelationalTablet
  -> Co.Timestamp
  -> [ColumnValue]
  -> Either String RelationalTablet
insertRow tablet timestamp row = do
  (primaryKeyRev, nonPrimaryKeys) <- extractPrimary (tablet ^. i'schema) row [] []
  let primaryKey = reverse primaryKeyRev
      (_, storage') = MVM.write (primaryKey, Nothing) (Just $ CV'Empty ()) timestamp (tablet ^. i'storage)
      storage'' = insertColumns storage' primaryKey nonPrimaryKeys
  return $ tablet & i'storage .~ storage''
  where
    extractPrimary schema' row' primaryKey nonPrimaryKeys =
      case (schema', row') of
        (((columnName, columnType, isPrimary):schemaRest), (columnValue:rowRest)) ->
          if checkTypeMatch columnType columnValue
            then
              if isPrimary
                then extractPrimary schemaRest rowRest (columnValue:primaryKey) nonPrimaryKeys
                else extractPrimary schemaRest rowRest primaryKey ((columnName, columnValue):nonPrimaryKeys)
            else Left $ "columnValue " ++ (show columnValue)
                     ++ " does not match columnType " ++ (show columnType)
        ([], []) -> Right (primaryKey, nonPrimaryKeys)
        _ -> Left $ "Number of entries in row " ++ (show row)
                 ++ " does not match the schema " ++ (show $ tablet ^. i'schema)
    insertColumns storage primaryKey nonPrimaryKeys =
      case nonPrimaryKeys of
        ((columnName, columnValue):rest) ->
          let (_, storage') = MVM.write (primaryKey, Just columnName) (Just columnValue) timestamp storage
          in insertColumns storage' primaryKey rest
        [] -> storage
