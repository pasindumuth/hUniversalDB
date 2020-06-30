module Slave.DerivedState (
  DS.DerivedState,
  handleDerivedState
) where

import qualified Control.Monad as Mo

import qualified Paxos.PaxosLog as PL
import qualified Proto.Messages.PaxosMessages as PM
import qualified Slave.Internal.DerivedState as DS
import Infra.State

handleDerivedState :: PL.PaxosLog -> PL.PaxosLog -> ST DS.DerivedState ()
handleDerivedState pl pl' = do
  Mo.forM_ (PL.newlyAddedEntries pl pl') $ \(index, plEntry) -> return ()
