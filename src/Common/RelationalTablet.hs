{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Common.RelationalTablet where

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
type TabletStorage = MVM.MultiVersionMap ([RT.ColumnValue], Maybe String) RT.ColumnValue
type Schema = [(String, RT.ColumnType, Bool)]

-- In this schema definition, the bool at the end indicates if the
-- if the column is a primary key column.
data RelationalTablet = RelationalTablet {
  _i'schema :: Schema,
  _i'storage :: TabletStorage
}

makeLenses ''RelationalTablet

-- Checks if the RT.ColumnValue has the type that's indicated by the RT.ColumnType.
checkTypeMatch :: RT.ColumnType -> RT.ColumnValue -> Bool
checkTypeMatch columnType columnValue =
  case (columnType, columnValue) of
    (RT.CT'String, RT.CV'String _) -> True
    (RT.CT'Int, RT.CV'Int _) -> True
    (RT.CT'Double, RT.CV'Double _) -> True
    (RT.CT'Bool, RT.CV'Bool _) -> True
    (RT.CT'Empty, RT.CV'Empty _) -> True
    _ -> False

------------------------------------------------------------------------------------------------------------------------
-- Backdoor Operations
------------------------------------------------------------------------------------------------------------------------
-- These operations are primarily meant for testing purposes to populate the tables
-- with test data. They aren't meant to be used in production, where careful
-- transaction processing takes place.

-- | A helper function that makes sure the primary key actually
-- conforms to the schema.
checkPrimaryKey
  :: Schema
  -> [RT.ColumnValue]
  -> Either String [RT.ColumnValue]
checkPrimaryKey schema primaryKey =
  case schema of
    ((columnName, columnType, isPrimary):r'schema) ->
      case (isPrimary, primaryKey) of
        (True, []) -> Left $ "Not enough elements the primaryKey."
        (True, (columnValue:r'primaryKey)) ->
          if checkTypeMatch columnType columnValue
            then checkPrimaryKey r'schema r'primaryKey
            else Left $ "columnValue " ++ (show columnValue)
                     ++ " does not match columnType " ++ (show columnType)
        (False, _) -> checkPrimaryKey r'schema primaryKey
    [] ->
      case primaryKey of
        (_:_) -> Left $ "Too many elements in the primaryKey."
        _ -> Right primaryKey

-- | Inserts the row into the RelationalTablet. This method assumes that this is
-- a valid operation; the timestamp is greater than the timestamps of all column
-- Lats for the given primary key, and a row with the given primary key doesn't
-- already exist.
insertRow
  :: RelationalTablet -- ^ The RelationalTablet to be modified.
  -> Co.Timestamp -- ^ The timestamp to insert.
  -> RT.Row -- ^ The full row, with an element for every column, in the
                   -- order corresponding to the tablet schema (otherwise an
                   -- error is returned).
  -> Either String RelationalTablet -- ^ An error or the modified Tablet.
insertRow tablet timestamp (RT.Row row) = do
  (primaryKeyRev, nonPrimaryKeys) <- extractPrimary (tablet ^. i'schema) row [] []
  let primaryKey = reverse primaryKeyRev
      (_, storage') = MVM.write (primaryKey, Nothing) (Just $ RT.CV'Empty ()) timestamp (tablet ^. i'storage)
      storage'' = insertColumns storage' primaryKey nonPrimaryKeys
  return $ tablet & i'storage .~ storage''
  where
    extractPrimary schema row primaryKey nonPrimaryKeys =
      case (schema, row) of
        (((columnName, columnType, isPrimary):r'schema), (columnValue:r'row)) ->
          if checkTypeMatch columnType columnValue
            then
              if isPrimary
                then extractPrimary r'schema r'row (columnValue:primaryKey) nonPrimaryKeys
                else extractPrimary r'schema r'row primaryKey ((columnName, columnValue):nonPrimaryKeys)
            else Left $ "columnValue " ++ (show columnValue)
                     ++ " does not match columnType " ++ (show columnType)
        ([], []) -> Right (primaryKey, nonPrimaryKeys)
        _ -> Left $ "Number of entries in row " ++ (show row)
                 ++ " does not match the schema " ++ (show $ tablet ^. i'schema)
    insertColumns storage primaryKey nonPrimaryKeys =
      case nonPrimaryKeys of
        ((columnName, columnValue):r'nonPrimaryKeys) ->
          let (_, storage') = MVM.write (primaryKey, Just columnName) (Just columnValue) timestamp storage
          in insertColumns storage' primaryKey r'nonPrimaryKeys
        [] -> storage

-- | Deletes a row from the RelationalTablet. This method assumes that this is
-- a valid operation; the timestamp is greater than the timestamps of all column
-- Lats for the given primary key, and a row with the given primary key
-- already exists.
deleteRow
  :: RelationalTablet -- ^ The RelationalTablet to be modified.
  -> Co.Timestamp -- ^ The timestamp to insert.
  -> RT.PrimaryKey -- ^ The primary key to delete. The elements must be sorted in the
                   -- same order that they appear in the table schema. (otherwise an
                   -- error is returned).
  -> Either String RelationalTablet -- ^ An error or the modified Tablet.
deleteRow tablet timestamp (RT.PrimaryKey primaryKey) = do
  primaryKey <- checkPrimaryKey (tablet ^. i'schema) primaryKey
  let nonPrimaryKeys = getNonPrimaryKeyNames (tablet ^. i'schema) []
      (_, storage') = MVM.write (primaryKey, Nothing) Nothing timestamp (tablet ^. i'storage)
      storage'' = U.fold storage' nonPrimaryKeys $ \storage' nonPrimaryKey ->
                    snd $ MVM.write (primaryKey, Just nonPrimaryKey) Nothing timestamp storage'
  return $ tablet & i'storage .~ storage''
  where
    getNonPrimaryKeyNames schema nonPrimaryKeyNames =
      case schema of
        ((columnName, _, isPrimary):r'schema) ->
          if isPrimary
            then getNonPrimaryKeyNames r'schema nonPrimaryKeyNames
            else getNonPrimaryKeyNames r'schema (columnName:nonPrimaryKeyNames)
        [] -> nonPrimaryKeyNames

-- | Updates a column in the RelationalTablet. This method assumes that this is
-- a valid operation; the timestamp is greater than the timestamps of all column
-- Lats for the given primary key.
updateColumn
  :: RelationalTablet -- ^ The RelationalTablet to be modified.
  -> Co.Timestamp -- ^ The timestamp to insert.
  -> RT.PrimaryKey -- ^ The primary key to update. The elements must be sorted in the
                   -- same order that they appear in the table schema. (otherwise an
                   -- error is returned).
  -> String -- ^ The name of the column to update
  -> Maybe RT.ColumnValue -- ^ The value to update to. Recall that a Nothing value
                       -- means NULL (in SQL terms).
  -> Either String RelationalTablet -- ^ An error or the modified Tablet.
updateColumn tablet timestamp (RT.PrimaryKey primaryKey) columnName columnValueM = do
  primaryKey <- checkPrimaryKey (tablet ^. i'schema) primaryKey
  let (_, storage') = MVM.write (primaryKey, Just columnName) columnValueM timestamp (tablet ^. i'storage)
  return $ tablet & i'storage .~ storage'
