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
import Control.Lens (makeLenses, (%~), (.~), (^.), (&), (?~))
import Control.Lens.Unsound (lensProduct)
import Control.Lens.At (at, ix)
import Text.Pretty.Simple (pPrintNoColor)

import qualified Connections as CC
import qualified Logging as L
import qualified MultiPaxos as MP
import qualified MessageHandler as MH
import qualified Message as M

for = flip map
foldl3 z t f = foldl f z t

type Queues = Mp.Map CC.EndpointId (Mp.Map CC.EndpointId (Sq.Seq M.Message))
type NonemptyQueues = St.Set (CC.EndpointId, CC.EndpointId)

data GlobalState = GlobalState {
  _slaveEIds :: [CC.EndpointId], -- EndpointIds for all slaves in the system
  _clientEIds :: [CC.EndpointId], -- EndpointIds for all client's we use for testing
  -- `queues` contains 2 queues (in for each direction) for every pair of
  -- for both client EndpointIds and slave Endpoints.
  _queues :: Queues,
  -- We use pairs of endpoints as identifiers of a queue. `nonemptyQueues`
  -- contain all queue IDs where the queue is non-empty
  _nonemptyQueues :: NonemptyQueues,
  _multiPaxosIs :: Mp.Map CC.EndpointId MP.MultiPaxos,
  _rand :: R.StdGen -- Random Number Generator for simulation
} deriving (Show)

makeLenses ''GlobalState

mkSlaveEId i = "s" ++ show i
mkClientEId i = "c" ++ show i

createGlobalState :: Int -> Int -> Int -> GlobalState
createGlobalState seed numSlaves numClients =
  let slaveEIds = for [0..(numSlaves - 1)] $ mkSlaveEId
      clientEIds = for [0..(numClients - 1)] $ mkClientEId
      eIds = slaveEIds ++ clientEIds
      queues = Mp.fromList $ for eIds $ \eid1 ->
        (eid1, Mp.fromList $ for eIds $ \eid2 ->
        (eid2, Sq.empty))
      nonemptyQueues = St.empty
      multiPaxosIs = Mp.fromList $ for eIds $ \eid -> (eid, D.def)
      rand = R.mkStdGen seed
   in GlobalState slaveEIds clientEIds queues nonemptyQueues multiPaxosIs rand

addMsg :: M.Message -> (CC.EndpointId, CC.EndpointId) -> (Queues, NonemptyQueues) -> (Queues, NonemptyQueues)
addMsg msg (fromEId, toEId) (queues, nonemptyQueues) =
  let queues' = queues & ix fromEId %~ ix toEId %~ (Sq.|> msg)
      nonemptyQueues' =
        if (Sq.length $ queues' ^. ix fromEId . ix toEId) == 1
          then nonemptyQueues & St.insert (fromEId, toEId)
          else nonemptyQueues
  in (queues', nonemptyQueues')

deliverMessage :: (CC.EndpointId, CC.EndpointId) -> GlobalState -> GlobalState
deliverMessage (fromEId, toEId) g =
   let msg Sq.:< queue' = Sq.viewl $ g ^. queues ^. ix fromEId . ix toEId
       queues' = g ^. queues & ix fromEId %~ ix toEId .~ queue'
       nonemptyQueues' = if (Sq.length queue') == 0
                           then g ^. nonemptyQueues & St.delete (fromEId, toEId)
                           else g ^. nonemptyQueues
       Just multiPaxosI = g ^. multiPaxosIs ^. at toEId
       mpMsg = MH.handleMessage msg
       (msgsO, (multiPaxosI', rand')) = MP.handleMultiPaxos (g ^. slaveEIds) fromEId mpMsg (multiPaxosI, g ^. rand)
       multiPaxosIs' = g ^. multiPaxosIs & ix toEId .~ multiPaxosI'
       (queues'', nonemptyQueues'') = foldl3 (queues', nonemptyQueues') msgsO $
         \(queues', nonemptyQueues') (eId, msgO) ->
           addMsg (M.MMessage msgO) (toEId, eId) (queues', nonemptyQueues')
   in g & queues .~ queues''
        & nonemptyQueues .~ nonemptyQueues''
        & multiPaxosIs .~ multiPaxosIs'
        & rand .~ rand'

simulate :: GlobalState -> GlobalState
simulate g =
  if (g ^. nonemptyQueues & St.size) == 0
    then g
    else
      let (randQueueIndex, rg') = R.randomR (0, g ^. nonemptyQueues & St.size & (subtract 1)) (g ^. rand)
          g' = g & rand .~ rg'
          queueId = g' ^. nonemptyQueues & St.elemAt randQueueIndex
          g'' = deliverMessage queueId g'
      in simulate g''

test1 :: GlobalState -> GlobalState
test1 g =
  let clientMsg = M.MMessage $ M.Insert $ M.Write "message"
      (clientEId, slaveEId) = (mkClientEId 0, mkSlaveEId 0)
      g' = g & lensProduct queues nonemptyQueues %~ addMsg clientMsg (clientEId, slaveEId)
  in simulate g'

test :: IO ()
test = do
  let g = createGlobalState 0 5 1
      g' = test1 g
  pPrintNoColor g'
  return ()
