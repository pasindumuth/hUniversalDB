{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Slave.DerivedState (
  DerivedState,
  keySpaceManager,
  handleDerivedState
) where

import qualified Control.Monad as Mo
import qualified Data.Default as Df
import qualified Data.Set as St
import qualified GHC.Generics as Gn

import qualified Infra.Utils as U
import qualified Proto.Actions.Actions as Ac
import qualified Paxos.PaxosLog as PL
import qualified Proto.Common as Co
import qualified Proto.Messages.PaxosMessages as PM
import qualified Proto.Messages.TraceMessages as TrM
import qualified Slave.KeySpaceManager as KSM
import Infra.State
import Infra.Lens

data DerivedState = DerivedState {
  _i'keySpaceManager :: KSM.KeySpaceManager
} deriving (Gn.Generic, Df.Default, Show)

makeLenses ''DerivedState

keySpaceManager :: Lens' DerivedState KSM.KeySpaceManager
keySpaceManager = i'keySpaceManager

handleDerivedState
  :: Co.PaxosId
  -> PL.PaxosLog
  -> PL.PaxosLog
  -> ST DerivedState ()
handleDerivedState paxosId pl pl' = do
  Mo.forM_ (PL.newlyAddedEntries pl pl') $ \(index, plEntry) -> do
    trace $ TrM.PaxosInsertion paxosId index plEntry
    case plEntry of
      PM.Slave slaveEntry ->
        case slaveEntry of
          PM.RangeRead _ timestamp -> do
            i'keySpaceManager .^^ KSM.read timestamp
            return ()
          PM.RangeWrite requestId timestamp ranges -> do
            i'keySpaceManager .^^ KSM.write timestamp requestId ranges
            addA $ Ac.Slave_CreateTablet ranges
      _ -> U.caseError
