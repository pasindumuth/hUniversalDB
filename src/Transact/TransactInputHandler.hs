module Transact.TransactInputHandler where

import Proto.Actions.TransactActions as TrA
import Transact.TransactState as TS
import Infra.Lens
import Infra.State

handleInputAction
  :: TrA.InputAction
  -> STTr TS.TransactState ()
handleInputAction input = return ()
