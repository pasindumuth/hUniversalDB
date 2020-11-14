{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Common.RelationalTablet where

import qualified Data.Map as Mp
import qualified GHC.Generics as Gn

import qualified Common.Model.RelationalTablet as RT
import qualified Common.MultiVersionMap as MVM
import qualified Infra.Utils as U
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

------------------------------------------------------------------------------------------------------------------------
-- Relational Tablet
------------------------------------------------------------------------------------------------------------------------
type TabletStorage = MVM.MultiVersionMap (RT.PrimaryKey, Maybe String) RT.ColumnValue

-- In this schema definition, the bool at the end indicates if the
-- if the column is a primary key column.
data RelationalTablet = RelationalTablet {
  _i'schema :: RT.Schema,
  _i'storage :: TabletStorage
}  deriving (Gn.Generic, Show)

makeLenses ''RelationalTablet

------------------------------------------------------------------------------------------------------------------------
-- Utilities Operations
------------------------------------------------------------------------------------------------------------------------
-- These are basic operations that don't necessarily manipulate a Tablet,
-- but are used while doing so.

-- | Checks if the RT.ColumnValue has the type that's indicated by the RT.ColumnType.
checkTypeMatch :: RT.ColumnType -> Maybe RT.ColumnValue -> Bool
checkTypeMatch columnType columnValueM =
  case columnValueM of
    Just columnValue ->
      case (columnType, columnValue) of
        (RT.CT'String, RT.CV'String _) -> True
        (RT.CT'Int, RT.CV'Int _) -> True
        (RT.CT'Double, RT.CV'Double _) -> True
        (RT.CT'Bool, RT.CV'Bool _) -> True
        (RT.CT'Empty, RT.CV'Empty _) -> True
        _ -> False
    Nothing -> True

checkRow
  :: RT.Row
  -> RT.Schema
  -> Either String RT.Row
checkRow (RT.Row rowKey rowColumns) schema = do
  primaryKey <- checkPrimaryKey rowKey schema
  let (RT.Schema _ schemaColumns) = schema
  r'rowColumns <- U.foldM rowColumns schemaColumns $
    \rowColumns (_, columnType) ->
      case rowColumns of
        (rowColumn:r'rowColumns) ->
          if checkTypeMatch columnType rowColumn
            then Right r'rowColumns
            else Left "Non-primary key column type doesn't match schema."
        [] -> Left "Too little non-primary key columns according to the schema."
  case r'rowColumns of
    (_:_) -> Left "Too many non-primary key columns according to the schema."
    _ -> Right $ RT.Row primaryKey rowColumns


-- | A helper function that makes sure the primary key actually
-- conforms to the schema.
checkPrimaryKey
  :: RT.PrimaryKey
  -> RT.Schema
  -> Either String RT.PrimaryKey
checkPrimaryKey (RT.PrimaryKey primaryKey) (RT.Schema schemaKeys _) = do
    r'primaryKey <- U.foldM primaryKey schemaKeys $
      \primaryKey (_, columnType) ->
        case primaryKey of
          (primaryKeyCol:r'primaryKey) ->
            if checkTypeMatch columnType (Just primaryKeyCol)
              then Right r'primaryKey
              else Left "Primary key column type doesn't match schema."
          [] -> Left "Primary key is shorter than the schema indicates."
    case r'primaryKey of
      (_:_) -> Left "Primary key is longer than the schema indicates."
      _ -> Right (RT.PrimaryKey primaryKey)

------------------------------------------------------------------------------------------------------------------------
-- Backdoor Operations
------------------------------------------------------------------------------------------------------------------------
-- These operations are primarily meant for testing purposes to populate the tables
-- with test data. They aren't meant to be used in production, where careful
-- transaction processing takes place.

-- | Inserts the row into the RelationalTablet. This method assumes that this is
-- a valid operation; the timestamp is greater than the timestamps of all column
-- Lats for the given primary key, and a row with the given primary key doesn't
-- already exist.
insertRow
  :: Co.Timestamp -- ^ The timestamp to insert.
  -> RT.Row -- ^ The full row, with an element for every column, in the
            -- order corresponding to the tablet schema (otherwise an
            -- error is returned).
  -> RelationalTablet -- ^ The RelationalTablet to be modified.
  -> Either String RelationalTablet -- ^ An error or the modified Tablet.
insertRow timestamp row tablet = do
  let (RT.Schema _ schemaColumns) = (tablet ^. i'schema)
  (RT.Row primaryKey rowColumns) <- checkRow row (tablet ^. i'schema)
  let (_, storage') = MVM.write (primaryKey, Nothing) (Just $ RT.CV'Empty ()) timestamp (tablet ^. i'storage)
      (_, storage'') = U.fold (rowColumns, storage') schemaColumns $
        \(rowColumns, storage) (columnName, _) ->
          case rowColumns of
            (rowColumn:r'rowColumns) ->
              let (_, storage') = MVM.write (primaryKey, Just columnName) rowColumn timestamp storage
              in (r'rowColumns, storage')
            _ -> error $ "rowColumns " ++ (show rowColumns)
                      ++ " should match up with the column names " ++ (show schemaColumns) 
  return $ tablet & i'storage .~ storage''

-- | Deletes a row from the RelationalTablet. This method assumes that this is
-- a valid operation; the timestamp is greater than the timestamps of all column
-- Lats for the given primary key, and a row with the given primary key
-- already exists.
deleteRow
  :: Co.Timestamp -- ^ The timestamp to insert.
  -> RT.PrimaryKey -- ^ The primary key to delete. The elements must be sorted in the
                   -- same order that they appear in the table schema. (otherwise an
                   -- error is returned).
  -> RelationalTablet -- ^ The RelationalTablet to be modified.
  -> Either String RelationalTablet -- ^ An error or the modified Tablet.
deleteRow timestamp primaryKey tablet = do
  let (RT.Schema _ schemaColumns) = (tablet ^. i'schema)
  primaryKey <- checkPrimaryKey primaryKey (tablet ^. i'schema)
  let (_, storage') = MVM.write (primaryKey, Nothing) Nothing timestamp (tablet ^. i'storage)
      storage'' = U.fold storage' schemaColumns $
        \storage (columnName, _) ->
          let (_, storage') = MVM.write (primaryKey, Just columnName) Nothing timestamp storage
          in storage'
  return $ tablet & i'storage .~ storage''

-- | Updates a column in the RelationalTablet. This method assumes that this is
-- a valid operation; the timestamp is greater than the timestamps of all column
-- Lats for the given primary key.
updateColumn
  :: Co.Timestamp -- ^ The timestamp to insert.
  -> RT.PrimaryKey -- ^ The primary key to update. The elements must be sorted in the
                   -- same order that they appear in the table schema. (otherwise an
                   -- error is returned).
  -> String -- ^ The name of the column to update
  -> Maybe RT.ColumnValue -- ^ The value to update to. Recall that a Nothing value
                          -- means NULL (in SQL terms).
  -> RelationalTablet -- ^ The RelationalTablet to be modified.
  -> Either String RelationalTablet -- ^ An error or the modified Tablet.
updateColumn timestamp primaryKey columnName columnValue tablet = do
  primaryKey <- checkPrimaryKey primaryKey (tablet ^. i'schema)
  let (_, storage') = MVM.write (primaryKey, Just columnName) columnValue timestamp (tablet ^. i'storage)
  return $ tablet & i'storage .~ storage'

-- | Updates a column in the RelationalTablet. This method assumes that this is
-- a valid operation; the timestamp is greater than the timestamps of all column
-- Lats for the given primary key.
readRow
  :: Co.Timestamp -- ^ The timestamp to insert.
  -> RT.PrimaryKey -- ^ The primary key to update. The elements must be sorted in the
                   -- same order that they appear in the table schema. (otherwise an
                   -- error is returned).
                          -- means NULL (in SQL terms).
  -> RelationalTablet -- ^ The RelationalTablet to be modified.
  -> Either String (Maybe RT.Row, RelationalTablet) -- ^ An error or the modified Tablet.
readRow timestamp primaryKey tablet = do
  let (RT.Schema _ schemaColumns) = (tablet ^. i'schema)
  primaryKey <- checkPrimaryKey primaryKey (tablet ^. i'schema)
  let (value, storage') = MVM.read (primaryKey, Nothing) timestamp (tablet ^. i'storage)
  Right $ case value of
    Just _ -> -- A row with the given key actually exists at this timestamp
      let (rowColumnsReverse, storage'') = U.fold ([], storage') schemaColumns $
            \(rowColumnsReverse, storage) (columnName, _) ->
              let (value, storage') = MVM.read (primaryKey, Just columnName) timestamp storage
                  value' = fmap fst value
              in ((value':rowColumnsReverse), storage')
      in (Just $ RT.Row primaryKey (reverse rowColumnsReverse), tablet & i'storage .~ storage'')
    Nothing -> (Nothing, tablet & i'storage .~ storage')
