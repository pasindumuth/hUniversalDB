{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Paxos.Internal.PaxosLog where

import qualified Data.Default as Df
import qualified Data.Map as Mp
import qualified Data.Set as S

import qualified Proto.Messages.PaxosMessages as M
import Lens

data PaxosLog = PaxosLog {
  _plog :: Mp.Map M.IndexT M.PaxosLogEntry,
  _availableIndices :: S.Set Int
} deriving (Show, Eq)

instance Df.Default PaxosLog where
  def = PaxosLog Mp.empty $ S.fromList [0]

makeLenses ''PaxosLog
