module Transact.Tablet.TabletInputHandler where

import qualified Transact.Model.Actions as Ac
import qualified Transact.Tablet.TabletState as TS
import Transact.Infra.State
import Infra.Lens

handleInputAction
  :: Ac.T'InputAction
  -> STT TS.TabletState ()
handleInputAction input = return ()
