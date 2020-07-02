{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Paxos.Tasks.Task where

import qualified Proto.Messages as Ms
import qualified Proto.Messages.PaxosMessages as PM
import Infra.State

data Task derivedStateT = Task {
  description :: String,
  tryHandling :: derivedStateT -> ST () Bool,
  done :: derivedStateT -> ST () (),
  createPLEntry :: derivedStateT -> PM.PaxosLogEntry,
  msgWrapper :: (PM.MultiPaxosMessage -> Ms.Message)
}

instance Show (Task derivedStateT) where
  show (Task description _ _ _ _) = description
