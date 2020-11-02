{-# LANGUAGE RankNTypes #-}

module Transact.Infra.State (
  module Transact.Infra.State,
  module Infra.StateWithTrace
) where

import Transact.Model.Actions as Ac
import Transact.Model.Message as Ms
import Infra.StateWithTrace

-- Specific versions of ST
type STS s a = ST Ac.S'OutputAction Ms.Tr'TraceMessage s a -- For Transact Server
type STT s a = ST Ac.T'OutputAction Ms.Tr'TraceMessage s a -- For Transact Tablet
