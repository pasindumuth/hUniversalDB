{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Paxos.Tasks.Internal_PaxosTaskManager where

import qualified Data.Default as Df
import qualified Data.Sequence as Sq
import qualified GHC.Generics as Gn

import qualified Paxos.Tasks.Task as Ta
import qualified Proto.Messages.PaxosMessages as PM
import Infra.Lens

data CurrentInsert a = CurrentInsert {
  _index :: Int,
  _entry :: PM.PaxosLogEntry,
  _task :: Ta.Task a
} deriving (Show)

data PaxosTaskManager a = PaxosTaskManager {
  _currentInsert :: Maybe (CurrentInsert a),
  _taskQueue :: Sq.Seq (Ta.Task a),
  _counter :: Int
} deriving (Gn.Generic, Df.Default, Show)

makeLenses ''CurrentInsert
makeLenses ''PaxosTaskManager
