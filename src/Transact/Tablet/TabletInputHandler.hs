module Transact.Tablet.TabletInputHandler where

import qualified Transact.Container.TabletActions as TA
import qualified Transact.Tablet.TabletState as TS
import Transact.Infra.State
import Infra.Lens

handleInputAction
  :: TA.InputAction
  -> STT TS.TabletState ()
handleInputAction input = return ()
