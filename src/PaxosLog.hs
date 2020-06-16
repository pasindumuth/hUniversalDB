{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module PaxosLog where

import qualified Data.Default as D
import qualified Data.Map as Mp
import qualified Data.Maybe as Mb
import qualified Control.Exception.Base as Ex
import qualified Data.Set as S

import qualified Message as M
import Lens (makeLenses, (%~), (.~), (^.), (&))

data PaxosLog = PaxosLog {
  _plog :: Mp.Map M.IndexT M.PaxosLogEntry,
  _availableIndices :: S.Set Int
} deriving (Show, Eq)

instance D.Default PaxosLog where
  def = PaxosLog Mp.empty $ S.fromList [0]

makeLenses ''PaxosLog

insert :: M.IndexT -> M.PaxosLogEntry -> PaxosLog -> PaxosLog
insert index val p =
  Ex.assert (Mb.isNothing $ p ^. plog & Mp.lookup index) $
  Ex.assert ((p ^. availableIndices & S.size) > 0) $
  let lastAvailableIndex = p ^. availableIndices & S.elemAt ((p ^. availableIndices & S.size) - 1)
      plog' = p ^. plog & Mp.insert index val
      availableIndices' =
        if index < lastAvailableIndex
          then p ^. availableIndices & S.delete index
          else if index == lastAvailableIndex
            then p ^. availableIndices & S.delete index & S.insert (index + 1)
            else p ^. availableIndices & S.union $ S.fromList $ [lastAvailableIndex + 1 .. index - 1] ++ [index + 1]
  in PaxosLog plog' availableIndices'

nextAvailableIndex :: PaxosLog -> M.IndexT
nextAvailableIndex p =
  Ex.assert ((p ^. availableIndices & S.size) > 0) $
  p ^. availableIndices & S.elemAt 0