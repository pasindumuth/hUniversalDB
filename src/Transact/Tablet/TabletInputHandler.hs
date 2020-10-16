module Transact.Tablet.TabletInputHandler where

import Proto.Actions.TransactTabletActions as TTA
import Transact.Tablet.TabletState as TTS
import Infra.Lens
import Infra.State

handleInputAction
  :: TTA.InputAction
  -> STTT TTS.TabletState ()
handleInputAction input = return ()
