{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module TestState where

import qualified Data.Map as Mp
import qualified Data.Set as St
import qualified Data.Sequence as Sq
import qualified System.Random as Rn

import qualified Transact.Model.Actions as Ac
import qualified Transact.Model.Common as Co
import qualified Transact.Model.Message as Ms
import qualified Transact.Server.ServerState as SS
import qualified Transact.Tablet.TabletState as TS
import Infra.Lens

type Queues = Mp.Map Co.EndpointId (Mp.Map Co.EndpointId (Sq.Seq Ms.Message))
type NonemptyQueues = St.Set (Co.EndpointId, Co.EndpointId)

data TestState = TestState {
  -- General
  _rand :: Rn.StdGen,
  _serverEIds :: [Co.EndpointId], -- EndpointIds for all servers in the system
  _clientEIds :: [Co.EndpointId], -- EndpointIds for all client's we use for testing
  -- `queues` contains 2 queues (in for each direction) for every pair of
  -- client EndpointIds and slave Endpoints.
  _queues :: Queues,
  -- We use pairs of endpoints as identifiers of a queue. `nonemptyQueues`
  -- contain all queue IDs where the queue is non-empty
  _nonemptyQueues :: NonemptyQueues,
  -- The following are all server state objects.
  _serverStates :: Mp.Map Co.EndpointId SS.ServerState,
  _tabletStates :: Mp.Map Co.EndpointId (Mp.Map Co.TabletShape TS.TabletState),
  -- The following are everything related to asynchronous computation
  -- done at a node.
  _serverAsyncQueues :: Mp.Map Co.EndpointId (Sq.Seq (Ac.S'InputAction, Int)),
  _tabletAsyncQueues :: Mp.Map Co.EndpointId (Mp.Map Co.TabletShape (Sq.Seq (Ac.T'InputAction, Int))),
  _clocks :: Mp.Map Co.EndpointId Int,
  -- Meta
  _nextInt :: Int,
  _trueTimestamp :: Int,
  -- Accumulated client responses
  _clientMsgsReceived :: St.Set Ms.Message
} deriving (Show)

makeLenses ''TestState
