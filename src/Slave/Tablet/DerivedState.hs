module Slave.Tablet.DerivedState (
  DS.DerivedState,
  handleDerivedState
) where

import qualified Control.Monad as Mo

import qualified Paxos.PaxosLog as PL
import qualified Proto.Common as Co
import qualified Proto.Messages.PaxosMessages as PM
import qualified Proto.Messages.TraceMessages as TrM
import qualified Slave.Tablet.Internal_DerivedState as DS
import qualified Slave.Tablet.MultiVersionKVStore as MS
import Infra.State

handleDerivedState :: Co.PaxosId -> PL.PaxosLog -> PL.PaxosLog -> ST DS.DerivedState ()
handleDerivedState paxosId pl pl' = do
  Mo.forM_ (PL.newlyAddedEntries pl pl') $ \(index, plEntry) -> do
    trace $ TrM.PaxosInsertion paxosId index plEntry
    case plEntry of
      PM.Tablet_Read key timestamp -> do
        DS.kvStore .^^ MS.read key timestamp
        return ()
      PM.Tablet_Write key value timestamp -> do
        DS.kvStore .^^ MS.write key value timestamp
