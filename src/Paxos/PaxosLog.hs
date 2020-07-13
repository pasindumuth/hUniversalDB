{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Paxos.PaxosLog (
  PaxosLog,
  insert,
  nextAvailableIndex,
  newlyAddedEntries,
  plog,
  getPLEntry,
) where

import qualified Control.Exception.Base as Ex
import qualified Data.Default as Df
import qualified Data.Map as Mp
import qualified Data.Maybe as Mb
import qualified Data.Set as S

import qualified Proto.Messages.PaxosMessages as M
import qualified Infra.Utils as U
import Infra.Lens

data PaxosLog = PaxosLog {
  _i'plog :: Mp.Map M.IndexT M.PaxosLogEntry,
  _i'availableIndices :: S.Set Int
} deriving (Show, Eq)

makeLenses ''PaxosLog

instance Df.Default PaxosLog where
  def = PaxosLog Mp.empty $ S.fromList [0]

insert :: M.IndexT -> M.PaxosLogEntry -> PaxosLog -> PaxosLog
insert index val p =
  Ex.assert (Mb.isNothing $ p ^. i'plog & Mp.lookup index) $
  Ex.assert ((p ^. i'availableIndices & S.size) > 0) $
  let lastAvailableIndex = p ^. i'availableIndices & S.elemAt ((p ^. i'availableIndices & S.size) - 1)
      plog' = p ^. i'plog & Mp.insert index val
      availableIndices' =
        if index < lastAvailableIndex
          then p ^. i'availableIndices & S.delete index
          else if index == lastAvailableIndex
            then p ^. i'availableIndices & S.delete index & S.insert (index + 1)
            else p ^. i'availableIndices & S.union $ S.fromList $ [lastAvailableIndex + 1 .. index - 1] ++ [index + 1]
  in PaxosLog plog' availableIndices'

nextAvailableIndex :: PaxosLog -> M.IndexT
nextAvailableIndex p =
  Ex.assert ((p ^. i'availableIndices & S.size) > 0) $
  p ^. i'availableIndices & S.elemAt 0

newlyAddedEntries :: PaxosLog -> PaxosLog -> [(M.IndexT, M.PaxosLogEntry)]
newlyAddedEntries pl pl' =
  let index = pl & nextAvailableIndex
      index' = pl' & nextAvailableIndex
  in U.for [index .. index' - 1] $ \i -> (i, pl' ^. i'plog . at i & Mb.fromJust)

plog :: PaxosLog -> Mp.Map M.IndexT M.PaxosLogEntry
plog pl = pl ^. i'plog

getPLEntry :: M.IndexT -> PaxosLog -> Maybe M.PaxosLogEntry
getPLEntry index pl = pl ^. i'plog . at index
