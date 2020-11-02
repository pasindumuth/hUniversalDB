{-# LANGUAGE RankNTypes #-}

module Infra.State (
  module Infra.State,
  module Infra.StateWithTrace
) where

import qualified Proto.Actions.MasterActions as MAc
import qualified Proto.Actions.SlaveActions as SAc
import qualified Proto.Actions.TabletActions as TAc
import qualified Proto.Messages.TraceMessages as TrM
import Infra.StateWithTrace

-- Specific versions of ST
type STM s a = ST MAc.OutputAction TrM.TraceMessage s a -- For Master
type STS s a = ST SAc.OutputAction TrM.TraceMessage s a -- For Slave
type STT s a = ST TAc.OutputAction TrM.TraceMessage s a -- For Tablet
