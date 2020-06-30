module Slave.Tablet.DerivedState (
  DS.DerivedState,
  handleDerivedState
) where

import qualified Control.Monad as Mo

import qualified Paxos.PaxosLog as PL
import qualified Proto.Messages.PaxosMessages as PM
import qualified Slave.Tablet.Internal_DerivedState as DS
import qualified Slave.Tablet.MultiVersionKVStore as MS
import Infra.State

handleDerivedState :: PL.PaxosLog -> PL.PaxosLog -> ST DS.DerivedState ()
handleDerivedState pl pl' = do
  Mo.forM_ (PL.newlyAddedEntries pl pl') $ \(index, plEntry) ->
    case plEntry of
      PM.Read key timestamp -> do
        DS.kvStore .^^ MS.read key timestamp
        return ()
      PM.Write key value timestamp -> do
        DS.kvStore .^^ MS.write key value timestamp
