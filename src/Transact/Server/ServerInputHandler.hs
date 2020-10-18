module Transact.Server.ServerInputHandler where

import qualified Transact.Container.ServerActions as SA
import qualified Transact.Server.ServerState as TS

import Transact.Infra.State
import Infra.Lens

handleInputAction
  :: SA.InputAction
  -> STS TS.ServerState ()
handleInputAction input = return ()
