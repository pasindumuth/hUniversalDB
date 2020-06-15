{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Test where

import qualified Data.Default as D
import qualified Data.Map as Mp
import qualified Data.Set as St
import qualified Data.Sequence as Sq
import qualified System.Environment as E
import qualified System.Random as R
import Control.Lens (makeLenses, (%~), (.~), (^.), (&), (?~), (^?!))
import Control.Lens.At (at, ix)

import qualified Connections as CC
import qualified Logging as L
import qualified MultiPaxos as MP
import qualified MessageHandler as MH
import qualified Message as M

for = flip map
foldl3 z t f = foldl f z t

data GlobalState = GlobalState {
  _slaveEIds :: [CC.EndpointId], -- EndpointIds for all slaves in the system
  _clientEIds :: [CC.EndpointId], -- EndpointIds for all client's we use for testing
  -- `queues` contains 2 queues (in for each direction) for every pair of
  -- for both client EndpointIds and slave Endpoints.
  _queues :: Mp.Map CC.EndpointId (Mp.Map CC.EndpointId (Sq.Seq M.Message)),
  -- We use pairs of endpoints as identifiers of a queue. `nonemptyQueues` 
  -- contain all queue IDs where the queue is non-empty
  _nonemptyQueues :: St.Set (CC.EndpointId, CC.EndpointId),
  _multiPaxosIs :: Mp.Map CC.EndpointId MP.MultiPaxos
}

makeLenses ''GlobalState

createGlobalState :: GlobalState
createGlobalState =
  let slaveEIds = for [0..4] $ \i -> "s" ++ show i
      clientEIds = for [0..0] $ \i -> "c" ++ show i
      eIds = slaveEIds ++ clientEIds
      queues = Mp.fromList $ for eIds $ \eid1 ->
        (eid1, Mp.fromList $ for eIds $ \eid2 ->
        (eid2, Sq.empty))
      nonemptyQueues = St.empty
      multiPaxosIs = Mp.fromList $ for eIds $ \eid -> (eid, D.def)
   in GlobalState slaveEIds clientEIds queues nonemptyQueues multiPaxosIs

deliverMessage :: GlobalState -> (CC.EndpointId, CC.EndpointId) -> GlobalState
deliverMessage g (fromEId, toEId) =
   let msg Sq.:< queue' = Sq.viewl $ g ^. queues ^. ix fromEId . ix toEId
       queues' = g ^. queues & ix fromEId %~ ix toEId .~ queue'
       nonemptyQueues' = if (Sq.length queue') == 0
                           then g ^. nonemptyQueues & St.delete (fromEId, toEId)
                           else g ^. nonemptyQueues
       Just multiPaxosI = (g ^. multiPaxosIs) ^. at toEId
       mpMsg = MH.handleMessage msg
       (msgsO, multiPaxosI') = MP.handleMultiPaxos multiPaxosI (g ^. slaveEIds) fromEId mpMsg
       multiPaxosIs' = g ^. multiPaxosIs & ix toEId .~ multiPaxosI'
       (queues'', nonemptyQueues'') = foldl3 (queues', nonemptyQueues') msgsO $
         \(queues', nonemptyQueues') (eId, msgO) ->
           let queues'' = queues' & ix toEId %~ ix eId %~ (Sq.|> M.MMessage msgO)
               nonemptyQueues'' =
                 if (Sq.length $ queues'' ^. ix toEId . ix eId) == 1
                   then nonemptyQueues'' & St.insert (toEId, eId)
                   else nonemptyQueues''
           in (queues'', nonemptyQueues')
   in g & queues .~ queues''
        & nonemptyQueues .~ nonemptyQueues''
        & multiPaxosIs .~ multiPaxosIs'

test :: IO ()
test = do
  return ()
