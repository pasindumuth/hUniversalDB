{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module TestState where

import qualified Data.Map as Mp
import qualified Data.Set as St
import qualified Data.Sequence as Sq
import qualified System.Random as Rn

import qualified Master.MasterState as MS
import qualified Proto.Actions.MasterActions as MAc
import qualified Proto.Actions.SlaveActions as SAc
import qualified Proto.Actions.TabletActions as TAc
import qualified Proto.Common as Co
import qualified Proto.Messages as Ms
import qualified Slave.SlaveState as SS
import qualified Slave.Tablet.TabletState as TS
import qualified ClientState as CS
import qualified RequestStats as RS
import Infra.Lens
import Infra.State

type Queues = Mp.Map Co.EndpointId (Mp.Map Co.EndpointId (Sq.Seq Ms.Message))
type NonemptyQueues = St.Set (Co.EndpointId, Co.EndpointId)

data TestState = TestState {
  -- General
  _rand :: Rn.StdGen,
  _masterEIds :: [Co.EndpointId], -- EndpointIds for all masters in the system
  _slaveEIds :: [Co.EndpointId], -- EndpointIds for all slaves in the system
  _clientEIds :: [Co.EndpointId], -- EndpointIds for all client's we use for testing
  -- `queues` contains 2 queues (in for each direction) for every pair of
  -- for both client EndpointIds and slave Endpoints.
  _queues :: Queues,
  -- We use pairs of endpoints as identifiers of a queue. `nonemptyQueues`
  -- contain all queue IDs where the queue is non-empty
  _nonemptyQueues :: NonemptyQueues,
  -- The following are all server state objects.
  _masterState :: Mp.Map Co.EndpointId MS.MasterState,
  _slaveState :: Mp.Map Co.EndpointId SS.SlaveState,
  _tabletStates :: Mp.Map Co.EndpointId (Mp.Map Co.KeySpaceRange TS.TabletState),
  -- The following are everything related to asynchronous computation
  -- done at a node.
  _masterAsyncQueues :: Mp.Map Co.EndpointId (Sq.Seq (MAc.InputAction, Int)),
  _slaveAsyncQueues :: Mp.Map Co.EndpointId (Sq.Seq (SAc.InputAction, Int)),
  _tabletAsyncQueues :: Mp.Map Co.EndpointId (Mp.Map Co.KeySpaceRange (Sq.Seq (TAc.InputAction, Int))),
  _clocks :: Mp.Map Co.EndpointId Int,
  -- Client field
  _clientState :: Mp.Map Co.EndpointId CS.ClientState,
  _nextUid :: Int,
  _trueTimestamp :: Int,
  _requestStats :: RS.RequestStats
} deriving (Show)

makeLenses ''TestState
