module Slave.DerivedStateHandler (
  handleDerivedState
) where

import qualified Control.Monad as Mo
import qualified Data.Set as St

import qualified Infra.Utils as U
import qualified Paxos.PaxosLog as PL
import qualified Proto.Actions.SlaveActions as SAc
import qualified Proto.Common as Co
import qualified Proto.Messages.PaxosMessages as PM
import qualified Proto.Messages.TraceMessages as TrM
import qualified Slave.KeySpaceManager as KSM
import qualified Slave.DerivedState as DS
import Infra.State
import Infra.Lens

handleDerivedState
  :: Co.PaxosId
  -> PL.PaxosLog
  -> PL.PaxosLog
  -> STS DS.DerivedState ()
handleDerivedState paxosId pl pl' = do
  Mo.forM_ (PL.newlyAddedEntries pl pl') $ \(index, plEntry) -> do
    trace $ TrM.PaxosInsertion paxosId index plEntry
    case plEntry of
      PM.Slave slaveEntry ->
        case slaveEntry of
          PM.RangeRead _ timestamp -> do
            DS.keySpaceManager .^^ KSM.read timestamp
            return ()
          PM.RangeWrite requestId timestamp ranges -> do
            DS.keySpaceManager .^^ KSM.write timestamp requestId ranges
            addA $ SAc.Slave_CreateTablet requestId ranges
      _ -> U.caseError
