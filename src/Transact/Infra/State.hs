{-# LANGUAGE RankNTypes #-}

module Transact.Infra.State (
  module Transact.Infra.State,
  module Infra.StateWithTrace
) where

import Transact.Container.ServerActions as SA
import Transact.Container.TabletActions as TA
import Infra.StateWithTrace

-- Specific versions of ST
type STS s a = ST SA.OutputAction s a -- For Transact Server
type STT s a = ST TA.OutputAction s a -- For Transact Tablet
