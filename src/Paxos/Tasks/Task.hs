{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Paxos.Tasks.Task where

import qualified Proto.Messages as Ms
import qualified Proto.Messages.PaxosMessages as PM
import qualified Proto.Messages.TraceMessages as TrM
import Infra.State

data Task derivedStateT outputActionT = Task {
  -- Used primarily for logging.
  description :: String,
  -- This is a stateless function that uses the derivedState to determine if the task
  -- should continue trying to insert into the PaxosLog. Often, it stops making sense
  -- to insert a particular PaxosLogEntry because doing so would result in an invalid
  -- derived state (if another insertion beat the first insertion attempt). If it should
  -- continue, then we should return Left with the PaxosLogEntry it wants to insert.
  -- Otherwise, it should return Right (), at which point the Task stops running
  -- (done won't be called).
  tryHandling :: derivedStateT -> ST outputActionT TrM.TraceMessage () (Either PM.PaxosLogEntry ()),
  -- This called after a tryHandling returns a Left, the PLEntry is proposed at index i,
  -- and then a PL insertion happens at i with an equal PLEntry. These callbacks usually
  -- send a network request or something related
  done :: derivedStateT -> ST outputActionT TrM.TraceMessage () (),
  -- This is used to wrap the PaxosLogEntry into a Ms.Message so that it's routed
  -- correctly to the right Paxos instances on the receiving end.
  msgWrapper :: (PM.MultiPaxosMessage -> Ms.Message)
}

instance Show (Task derivedStateT outputActionT) where
  show (Task description _ _ _) = description
