module Transact.Tablet.TabletInputHandler where

import qualified Common.Model.RelationalTablet as RTT
import qualified Common.RelationalTablet as RT
import qualified Transact.Model.Actions as Ac
import qualified Transact.Model.Message as Ms
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
        Ms.Admin (Ms.Ad'Message payload) ->
          case payload of
            Ms.Ad'Request' request -> do
              case request of
                Ms.Ad'Insert path row timestamp -> do
                  TS.relationalTablet .^^. \relationalTablet ->
                    case RT.insertRow timestamp row relationalTablet of
                      Right relationalTablet -> relationalTablet
                      Left errMsg -> error errMsg
                  return ()
                Ms.Ad'Update path primaryKey columnName columnValueM timestamp -> do
                  TS.relationalTablet .^^. \relationalTablet ->
                    case RT.updateColumn timestamp primaryKey columnName columnValueM relationalTablet of
                      Right relationalTablet -> relationalTablet
                      Left errMsg -> error errMsg
                  return ()
                Ms.Ad'Delete path primaryKey timestamp -> do
                  TS.relationalTablet .^^. \relationalTablet ->
                    case RT.deleteRow timestamp primaryKey relationalTablet of
                      Right relationalTablet -> relationalTablet
                      Left errMsg -> error errMsg
                  return ()
            Ms.Ad'Response' _ ->
              error "Admin responses shouldn't never be received."
