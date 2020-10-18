{-# LANGUAGE RankNTypes #-}

module Infra.State (
  module Infra.State,
  module Infra.StateWithTrace
) where

import qualified Proto.Actions.MasterActions as MAc
import qualified Proto.Actions.SlaveActions as SAc
import qualified Proto.Actions.TabletActions as TAc
import Infra.StateWithTrace

-- Specific versions of ST
type STM s a = ST MAc.OutputAction s a -- For Master
type STS s a = ST SAc.OutputAction s a -- For Slave
type STT s a = ST TAc.OutputAction s a -- For Tablet
