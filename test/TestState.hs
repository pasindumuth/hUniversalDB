{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module TestState where

import qualified Data.Map as Mp
import qualified Data.Set as St
import qualified Data.Sequence as Sq
import qualified System.Random as Rn

import qualified Proto.Actions.Actions as Ac
import qualified Proto.Common as Co
import qualified Proto.Messages as Ms
import qualified Slave.SlaveState as SS
import qualified Slave.Tablet.TabletState as TS
import Infra.Lens
import Infra.State

type Queues = Mp.Map Co.EndpointId (Mp.Map Co.EndpointId (Sq.Seq Ms.Message))
type NonemptyQueues = St.Set (Co.EndpointId, Co.EndpointId)

data TestState = TestState {
  _slaveEIds :: [Co.EndpointId], -- EndpointIds for all slaves in the system
  _clientEIds :: [Co.EndpointId], -- EndpointIds for all client's we use for testing
  -- `queues` contains 2 queues (in for each direction) for every pair of
  -- for both client EndpointIds and slave Endpoints.
  _queues :: Queues,
  -- We use pairs of endpoints as identifiers of a queue. `nonemptyQueues`
  -- contain all queue IDs where the queue is non-empty
  _nonemptyQueues :: NonemptyQueues,
  _slaveState :: Mp.Map Co.EndpointId SS.SlaveState,
  _tabletStates :: Mp.Map Co.EndpointId (Mp.Map Co.KeySpaceRange TS.TabletState),
  _rand :: Rn.StdGen, -- Random Number Generator for simulation

  -- The following are everything related to asynchronous computation
  -- done at a node.
  _slaveAsyncQueues :: Mp.Map Co.EndpointId (Sq.Seq (Ac.InputAction, Int)),
  _tabletAsyncQueues :: Mp.Map Co.EndpointId (Mp.Map Co.KeySpaceRange (Sq.Seq (Ac.TabletInputAction, Int))),
  _clocks :: Mp.Map Co.EndpointId Int,
  
  -- Fields for usage by the client
  _nextUid :: Int
} deriving (Show)

makeLenses ''TestState
