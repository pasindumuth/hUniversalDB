{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module PaxosLog where

import qualified Data.Default as D
import qualified Data.Map as Mp
import qualified Data.Maybe as Mb
import qualified Control.Exception.Base as Ex
import qualified Data.Set as S

import qualified Message as M

data PaxosLog = PaxosLog {
  log :: Mp.Map M.IndexT M.PaxosLogEntry,
  availableIndices :: S.Set Int
}

instance D.Default PaxosLog where
  def = PaxosLog {
      log = Mp.empty,
      availableIndices = S.fromList [0]
    }

insert :: PaxosLog -> M.IndexT -> M.PaxosLogEntry -> PaxosLog
insert paxosLog@PaxosLog{..} index val =
  Ex.assert (Mb.isNothing $ Mp.lookup index log) $
  Ex.assert ((S.size availableIndices) > 0) $
  let lastAvailableIndex = S.elemAt ((S.size availableIndices) - 1) availableIndices
      newLog = Mp.insert index val log
      newAvailableIndices =
        if index < lastAvailableIndex
          then S.delete index availableIndices
          else if index == lastAvailableIndex
            then S.insert (index + 1) $ S.delete index availableIndices
            else S.union availableIndices $ S.fromList $ [lastAvailableIndex + 1 .. index - 1] ++ [index + 1]
  in PaxosLog {
    log = newLog,
    availableIndices = newAvailableIndices
  }

nextAvailableIndex :: PaxosLog -> M.IndexT
nextAvailableIndex PaxosLog{..} =
  Ex.assert ((S.size availableIndices) > 0) $
  S.elemAt 0 availableIndices