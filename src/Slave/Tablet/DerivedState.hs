{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Slave.Tablet.DerivedState (
  DerivedState,
  kvStore,
  handleDerivedState
) where

import qualified Control.Monad as Mo
import qualified Data.Default as Df
import qualified GHC.Generics as Gn

import qualified Infra.Utils as U
import qualified Paxos.PaxosLog as PL
import qualified Proto.Common as Co
import qualified Proto.Messages.PaxosMessages as PM
import qualified Proto.Messages.TraceMessages as TrM
import qualified Slave.Tablet.MultiVersionKVStore as MVS
import Infra.State
import Infra.Lens

data DerivedState = DerivedState {
  _i'kvStore :: MVS.MultiVersionKVStore
} deriving (Gn.Generic, Df.Default, Show)

makeLenses ''DerivedState

kvStore :: Lens' DerivedState MVS.MultiVersionKVStore
kvStore = i'kvStore

handleDerivedState
  :: Co.PaxosId
  -> PL.PaxosLog
  -> PL.PaxosLog
  -> STT DerivedState ()
handleDerivedState paxosId pl pl' = do
  Mo.forM_ (PL.newlyAddedEntries pl pl') $ \(index, plEntry) -> do
    trace $ TrM.PaxosInsertion paxosId index plEntry
    case plEntry of
      PM.Tablet tabletEntry ->
        case tabletEntry of
          PM.Read _ key timestamp -> do
            i'kvStore .^^ MVS.read key timestamp
            return ()
          PM.Write requestId key value timestamp -> do
            i'kvStore .^^ MVS.write key requestId value timestamp
      _ -> U.caseError
