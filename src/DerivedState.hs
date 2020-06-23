module DerivedState (
  DS.DerivedState,
  handleDerivedState
) where

import qualified Records.DerivedState as DS
import qualified PaxosLog as PL

handleDerivedState :: PL.PaxosLog -> PL.PaxosLog -> DS.DerivedState -> DS.DerivedState
handleDerivedState pl pl' state = state
