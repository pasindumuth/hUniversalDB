module Slave.Tablet.DerivedState (
  DS.DerivedState,
  DS.kvStore,
  handleDerivedState
) where

import qualified Control.Monad as Mo

import qualified Infra.Utils as U
import qualified Paxos.PaxosLog as PL
import qualified Proto.Common as Co
import qualified Proto.Messages.PaxosMessages as PM
import qualified Proto.Messages.TraceMessages as TrM
import qualified Slave.Tablet.Internal_DerivedState as DS
import qualified Slave.Tablet.MultiVersionKVStore as MVS
import Infra.State

handleDerivedState :: Co.PaxosId -> PL.PaxosLog -> PL.PaxosLog -> ST DS.DerivedState ()
handleDerivedState paxosId pl pl' = do
  Mo.forM_ (PL.newlyAddedEntries pl pl') $ \(index, plEntry) -> do
    trace $ TrM.PaxosInsertion paxosId index plEntry
    case plEntry of
      PM.Tablet tabletEntry ->
        case tabletEntry of
          PM.Read _ key timestamp -> do
            DS.kvStore .^^ MVS.read key timestamp
            return ()
          PM.Write requestId key value timestamp -> do
            DS.kvStore .^^ MVS.write key requestId value timestamp
      _ -> U.caseError
