module Slave.DerivedState (
  IDS.DerivedState,
  handleDerivedState
) where

import qualified Control.Monad as Mo

import qualified Proto.Actions.Actions as Ac
import qualified Paxos.PaxosLog as PL
import qualified Proto.Common as Co
import qualified Proto.Messages.PaxosMessages as PM
import qualified Proto.Messages.TraceMessages as TrM
import qualified Slave.Internal_DerivedState as IDS
import qualified Slave.Internal_KeySpaceManager as IKSM
import Infra.State

handleDerivedState :: Co.PaxosId -> PL.PaxosLog -> PL.PaxosLog -> ST IDS.DerivedState ()
handleDerivedState paxosId pl pl' = do
  Mo.forM_ (PL.newlyAddedEntries pl pl') $ \(index, plEntry) -> do
    trace $ TrM.PaxosInsertion paxosId index plEntry
    case plEntry of
      PM.Slave_AddRange newRange generation -> do
        IDS.keySpaceManager.IKSM.ranges .^^. (newRange:)
        IDS.keySpaceManager.IKSM.generation .^^. \_ -> generation
        addA $ Ac.Slave_CreateTablet newRange
