{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Records.PaxosLog where

import qualified Data.Default as D
import qualified Data.Map as Mp
import qualified Data.Set as S

import qualified Records.Messages.PaxosMessages as M
import Lens (makeLenses)

data PaxosLog = PaxosLog {
  _plog :: Mp.Map M.IndexT M.PaxosLogEntry,
  _availableIndices :: S.Set Int
} deriving (Show, Eq)

instance D.Default PaxosLog where
  def = PaxosLog Mp.empty $ S.fromList [0]

makeLenses ''PaxosLog
