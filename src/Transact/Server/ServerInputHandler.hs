module Transact.Server.ServerInputHandler where

import qualified Data.List as Li
import qualified Data.Maybe as Ma

import qualified Common.Model.RelationalTablet as RTT
import qualified Common.RelationalTablet as RT
import qualified Transact.Model.Actions as Ac
import qualified Transact.Model.Common as Co
import qualified Transact.Model.Message as Ms
import qualified Transact.Model.SqlAST as Sql
import qualified Transact.Server.ServerState as SS
import qualified Infra.Utils as U

import Transact.Infra.State
import Infra.Lens

-- | Handles an input action by unwrapping it and passing the result to
-- some other specialized function.
handleInputAction
  :: Ac.S'InputAction
  -> STS SS.ServerState ()
handleInputAction input =
  case input of
    Ac.S'Receive eId msg ->
      case msg of
        Ms.Client _ -> error "Client requests not handled yet."
        Ms.Forwarded _ -> error "Forwarding not handled yet."
        Ms.Admin (Ms.Ad'Message _ payload) ->
          case payload of
            Ms.Ad'Request' request -> do
              res <- case request of
                Ms.Ad'InsertRq path row _ ->
                  SS.shapesWithSchema .^^^ findTabletShape path row
                Ms.Ad'UpdateRq path primaryKey _ _ _ ->
                  SS.shapesWithSchema .^^^ findTabletShapeWithPrimary path primaryKey
                Ms.Ad'DeleteRq path primaryKey _ ->
                  SS.shapesWithSchema .^^^ findTabletShapeWithPrimary path primaryKey
                Ms.Ad'ReadRowRq path primaryKey _ ->
                  SS.shapesWithSchema .^^^ findTabletShapeWithPrimary path primaryKey
                Ms.Ad'Select sqlStatement ->
                  SS.shapesWithSchema .^^^ findTabletIfFullTable sqlStatement
              case res of
                Right tabletShape -> addA $ Ac.S'TabletForward tabletShape eId msg
                Left errMsg -> error $ "Admin shouldn't sent a message that a slave can't handle. Error: " ++ errMsg
            Ms.Ad'Response' _ ->
              error "Admin responses shouldn't never be received."

-- | Extracts the table name that the SQL statement is referring to.
getTable :: Sql.SqlStatement -> String
getTable sqlStatement =
  case sqlStatement of
    Sql.SelectStatement _ fromBody _ ->
      let (Sql.FromBody (Sql.TableNameAlias table _)) = fromBody
      in table

-- | Analyzes the SQL statement to figure out what table that it needs
-- to touch, and then checks if this node manages that whole table,
-- returning true if it does and false otherwise.
findTabletIfFullTable
  :: Sql.SqlStatement
  -> [(RTT.Schema, Co.TabletShape)]
  -> Either String Co.TabletShape
findTabletIfFullTable sqlStatement shapesWithSchema =
  let targetShape = Co.TabletShape (Co.TabletPath $ getTable sqlStatement) (Co.TabletKeyRange Nothing Nothing)
  in case Li.find (\(_, shape) -> shape == targetShape) shapesWithSchema of
       Just _ -> Right targetShape
       Nothing -> Left $ "This node either doesn't have even a single Tablet of the table, "
                      ++ "or if it does, it doesn't manage the whole keyspace."

-- | Checks if the PrimaryKey is within the bounds of TabletKeyRange
-- (look at TabletKeyRange's docs). Importantly, note that this function
-- doesn't verify that the the elements in PrimaryKey's don't have
-- matching RTT.ColumnTypes.
isInRange
  :: RTT.PrimaryKey
  -> Co.TabletKeyRange
  -> Bool
isInRange primaryKey range =
  case range of
    (Co.TabletKeyRange (Just startKey) (Just endKey)) ->
      startKey <= primaryKey && primaryKey < endKey
    (Co.TabletKeyRange Nothing (Just endKey)) ->
      primaryKey < endKey
    (Co.TabletKeyRange (Just startKey) Nothing) ->
      startKey <= primaryKey
    (Co.TabletKeyRange Nothing Nothing) -> True

-- | This searches for a TabletShape with the given TabletPath. Once it finds
-- one, the given Row must match the corresponding Schema, otherwise we
-- throw an error. If we find a TabletShape containing the row, we return it.
-- Otherwise we also return an error.
findTabletShape
  :: Co.TabletPath
  -> RTT.Row
  -> [(RTT.Schema, Co.TabletShape)]
  -> Either String Co.TabletShape
findTabletShape tabletPath row shapesWithSchema =
  let findResult =
        U.foldM () shapesWithSchema $ \_ shapeWithSchema ->
          let (schema, tabletShape) = shapeWithSchema
              (Co.TabletShape path range) = tabletShape
          in if tabletPath == path
            then
              case RT.checkRow row schema of
                Right (RTT.Row primaryKey _) ->
                  if isInRange primaryKey range
                    then U.Break $ Right tabletShape
                    else U.Continue ()
                Left _ -> U.Break $ Left $ "The row " ++ (show row) ++ " does not conform the schema "
                                        ++ (show schema) ++ " of a TabletShape with the given path."
           else U.Continue ()
  in case findResult of
    U.Continue () -> Left "Couldn't find a TabletShape that can hold the given row."
    U.Break shapeWithSchemaE -> shapeWithSchemaE

-- | This searches for a TabletShape with the given TabletPath. Once it finds
-- one, the given PrimaryKey must match the corresponding Schema, otherwise we
-- throw an error. If we find a TabletShape containing the PrimaryKey, we
-- return it. Otherwise we also return an error.
findTabletShapeWithPrimary
  :: Co.TabletPath
  -> RTT.PrimaryKey
  -> [(RTT.Schema, Co.TabletShape)]
  -> Either String Co.TabletShape
findTabletShapeWithPrimary tabletPath primaryKey shapesWithSchema =
  let findResult =
        U.foldM () shapesWithSchema $ \_ shapeWithSchema ->
          let (schema, tabletShape) = shapeWithSchema
              (Co.TabletShape path range) = tabletShape
          in if tabletPath == path
            then
              case RT.checkPrimaryKey primaryKey schema of
                Right primaryKey ->
                  if isInRange primaryKey range
                    then U.Break $ Right tabletShape
                    else U.Continue ()
                Left _ -> U.Break $ Left $ "The row does not conform the schema "
                                        ++ "of a TabletShape with the given path."
            else U.Continue ()
  in case findResult of
    U.Continue () -> Left "Couldn't find a TabletShape that has the primary key."
    U.Break shapeWithSchemaE -> shapeWithSchemaE
