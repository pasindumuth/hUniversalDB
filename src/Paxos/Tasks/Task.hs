{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Paxos.Tasks.Task where

import qualified Proto.Messages as Ms
import qualified Proto.Messages.PaxosMessages as PM
import Infra.State

data Task derivedStateT outputActionT = Task {
  description :: String,
  tryHandling :: derivedStateT -> ST outputActionT () Bool,
  done :: derivedStateT -> ST outputActionT () (),
  createPLEntry :: derivedStateT -> PM.PaxosLogEntry,
  msgWrapper :: (PM.MultiPaxosMessage -> Ms.Message)
}

instance Show (Task derivedStateT outputActionT) where
  show (Task description _ _ _ _) = description
