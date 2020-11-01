module Transact.Server.ServerInputHandler where

import qualified Transact.Model.Actions as Ac
import qualified Transact.Server.ServerState as TS

import Transact.Infra.State
import Infra.Lens

handleInputAction
  :: Ac.S'InputAction
  -> STS TS.ServerState ()
handleInputAction input = return ()
