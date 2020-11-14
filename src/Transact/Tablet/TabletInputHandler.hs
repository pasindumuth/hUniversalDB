module Transact.Tablet.TabletInputHandler where

import qualified Data.Map as Mp

import qualified Common.Model.RelationalTablet as RTT
import qualified Common.RelationalTablet as RT
import qualified Infra.Utils as U
import qualified Transact.Model.Actions as Ac
import qualified Transact.Model.Message as Ms
import qualified Transact.Model.SqlAST as Sql
import qualified Transact.Tablet.TabletState as TS
import Transact.Infra.State
import Infra.Lens

handleInputAction
  :: Ac.T'InputAction
  -> STT TS.TabletState ()
handleInputAction input =
  case input of
    Ac.T'Receive eId msg ->
      case msg of
        Ms.Client _ -> error "Client requests not handled yet."
        Ms.Forwarded _ -> error "Forwarding not handled yet."
        Ms.Admin (Ms.Ad'Message (Ms.Ad'Metadata requestId) payload) ->
          case payload of
            Ms.Ad'Request' request -> do
              case request of
                Ms.Ad'InsertRq path row timestamp -> do
                  TS.relationalTablet .^^. \relationalTablet ->
                    case RT.insertRow timestamp row relationalTablet of
                      Right relationalTablet -> relationalTablet
                      Left errMsg -> error errMsg
                  return ()
                Ms.Ad'UpdateRq path primaryKey columnName columnValueM timestamp -> do
                  TS.relationalTablet .^^. \relationalTablet ->
                    case RT.updateColumn timestamp primaryKey columnName columnValueM relationalTablet of
                      Right relationalTablet -> relationalTablet
                      Left errMsg -> error errMsg
                  return ()
                Ms.Ad'DeleteRq path primaryKey timestamp -> do
                  TS.relationalTablet .^^. \relationalTablet ->
                    case RT.deleteRow timestamp primaryKey relationalTablet of
                      Right relationalTablet -> relationalTablet
                      Left errMsg -> error errMsg
                  return ()
                Ms.Ad'ReadRowRq path primaryKey timestamp -> do
                  rowM <- TS.relationalTablet .^^ \relationalTablet ->
                    case RT.readRow timestamp primaryKey relationalTablet of
                      Right (rowM, relationalTablet) -> (rowM, relationalTablet)
                      Left errMsg -> error errMsg
                  addA $ Ac.T'Send [eId] $ Ms.Admin $
                    Ms.Ad'Message
                      (Ms.Ad'Metadata requestId)
                      (Ms.Ad'Response' $ Ms.Ad'ReadRowRs rowM timestamp)
                  return ()
                Ms.Ad'Select sqlStatement -> do
                  -- For now, we don't do a snapshot-read at a given time. Instead
                  -- we take every row at the lat of it's key, and then run the
                  -- WHERE clause for that.
                  rowsE <- TS.relationalTablet .^^^ processQuery sqlStatement
                  case rowsE of
                    Right rows -> do
                      addA $ Ac.T'Send [eId] $ Ms.Admin $
                        Ms.Ad'Message
                          (Ms.Ad'Metadata requestId)
                          (Ms.Ad'Response' $ Ms.Ad'SelectRs rows)
                      return ()
                    Left errMsg -> error $ "Couldn't execute SQL statement: " ++ errMsg
            Ms.Ad'Response' _ ->
              error "Admin responses shouldn't never be received."

-- | Simply returns all rows (that are present, i.e. the
-- primaryKey is not Nothing) a the lat of the key.
processQuery
  :: Sql.SqlStatement
  -> RT.RelationalTablet
  -> Either String [RTT.Row]
processQuery _ tablet = do
  let (RT.RelationalTablet _ storage) = tablet
      keysWithLat = -- all keys in `storage`, along with the lat
        U.s31 Mp.foldlWithKey [] storage $
          \keysWithLat primaryKeyColumn (_, lat) ->
            case primaryKeyColumn of
              -- Here, we encouter a new key that now a non-Nothing value
              -- for it's latest value. Here, we extract the associated
              -- timestamp as well.
              (primaryKey, Nothing) -> (primaryKey, lat):keysWithLat
              _ -> keysWithLat
  U.foldM [] keysWithLat $
    \rows (primaryKey, lat) -> do
      res <- RT.readRow lat primaryKey tablet
      case res of
        -- If the row is actually present (it's possible that latest update to
        -- a row was its deletion, then add it to the list of rows that we are
        -- going to return.
        (Just row, _) -> Right $ row:rows
        (Nothing, _) -> Right rows
