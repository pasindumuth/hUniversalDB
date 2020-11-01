module Transact.Server.ServerInputHandler where

import qualified Common.RelationalTablet as RT
import qualified Transact.Model.Actions as Ac
import qualified Transact.Model.Message as Ms
import qualified Transact.Server.ServerState as TS
import qualified Infra.Utils as U

import Transact.Infra.State
import Infra.Lens

-- | Handles an input action by unwrapping it and passing the result to
-- some other specialized function.
handleInputAction
  :: Ac.S'InputAction
  -> STS TS.ServerState ()
handleInputAction input =
  case input of
    Ac.S'Receive eId msg ->
      case msg of
        Ms.Admin (Ms.Ad'Message payload) ->
          case payload of
            Ms.Ad'Request' request ->
              case request of
                Ms.Ad'Insert path row -> return ()
                Ms.Ad'Update path primaryKey columnName columnValue -> return ()
                Ms.Ad'Delete path primaryKey -> return ()
            Ms.Ad'Response' response
              -> U.caseError
        _ -> U.caseError
