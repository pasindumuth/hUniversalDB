module DerivedState (
  DS.DerivedState,
  handleDerivedState
) where

import qualified Control.Monad as Mo
import qualified Records.DerivedState as DS
import qualified MultiVersionKVStore as MS
import qualified PaxosLog as PL
import qualified Records.Messages.PaxosMessages as PM
import State (ST, get, (.^), (.^^), (.^^.))

handleDerivedState :: PL.PaxosLog -> PL.PaxosLog -> ST DS.DerivedState ()
handleDerivedState pl pl' = do
  Mo.forM_ (PL.newlyAddedEntries pl pl') $ \(index, plEntry) ->
    case plEntry of
      PM.Read key timestamp -> do
        DS.kvStore .^^ MS.read key timestamp
        return ()
      PM.Write key value timestamp -> do
        DS.kvStore .^^ MS.write key value timestamp
