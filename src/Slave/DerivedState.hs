module Slave.DerivedState (
  IDS.DerivedState,
  IDS.keySpaceManager,
  handleDerivedState
) where

import qualified Control.Monad as Mo
import qualified Data.Set as St

import qualified Infra.Utils as U
import qualified Proto.Actions.Actions as Ac
import qualified Paxos.PaxosLog as PL
import qualified Proto.Common as Co
import qualified Proto.Messages.PaxosMessages as PM
import qualified Proto.Messages.TraceMessages as TrM
import qualified Slave.Internal_DerivedState as IDS
import qualified Slave.KeySpaceManager as KSM
import Infra.State

handleDerivedState :: Co.PaxosId -> PL.PaxosLog -> PL.PaxosLog -> ST IDS.DerivedState ()
handleDerivedState paxosId pl pl' = do
  Mo.forM_ (PL.newlyAddedEntries pl pl') $ \(index, plEntry) -> do
    trace $ TrM.PaxosInsertion paxosId index plEntry
    case plEntry of
      PM.Slave slaveEntry ->
        case slaveEntry of
          PM.RangeRead _ timestamp -> do
            IDS.keySpaceManager .^^ KSM.read timestamp
            return ()
          PM.RangeWrite requestId timestamp ranges -> do
            IDS.keySpaceManager .^^ KSM.write timestamp requestId ranges
            addA $ Ac.Slave_CreateTablet ranges
      _ -> U.caseError
