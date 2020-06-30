{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Paxos.Tasks.Task where

import qualified Proto.Messages.PaxosMessages as PM
import Infra.State

data Task a = Task {
  description :: String,
  tryHandling :: a -> ST () Bool,
  done :: a -> ST () (),
  createPLEntry :: a -> PM.PaxosLogEntry
}

instance Show (Task a) where
  show (Task description _ _ _) = description
