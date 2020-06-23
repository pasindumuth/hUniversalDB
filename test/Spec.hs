{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Data.Default as D
import qualified Data.Map as Mp
import qualified Data.Set as St
import qualified Data.Sequence as Sq
import qualified System.Environment as E
import qualified System.Random as R
import Text.Pretty.Simple (pPrintNoColor)

import qualified Connections as CC
import qualified Logging as L
import qualified MultiPaxosInstance as MP
import qualified PaxosLog as PL
import qualified MessageHandler as MH
import qualified Records.Actions.Actions as A
import qualified Records.GlobalState as GS
import qualified Records.Env as EN
import qualified Records.Messages.ClientMessages as CM
import qualified Records.Messages.Messages as M
import qualified Records.Messages.PaxosMessages as PM
import qualified Utils as U
import Lens (makeLenses, (%~), (.~), (^.), (&), (?~), at, ix, lp2, lp3, _1, _2, (.^.))
import State (runST)
import qualified State as St

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
  _slaveState :: Mp.Map CC.EndpointId GS.GlobalState,
  _rand :: R.StdGen -- Random Number Generator for simulation
} deriving (Show)

makeLenses ''GlobalState

mkSlaveEId i = "s" ++ show i
mkClientEId i = "c" ++ show i

createGlobalState :: Int -> Int -> Int -> GlobalState
createGlobalState seed numSlaves numClients =
  let slaveEIds = U.for [0..(numSlaves - 1)] $ mkSlaveEId
      clientEIds = U.for [0..(numClients - 1)] $ mkClientEId
      eIds = slaveEIds ++ clientEIds
      queues = Mp.fromList $ U.for eIds $ \eid1 ->
        (eid1, Mp.fromList $ U.for eIds $ \eid2 ->
        (eid2, Sq.empty))
      nonemptyQueues = St.empty
      rand = R.mkStdGen seed
      (slaves, rand') = U.s13 foldl ([], rand) slaveEIds $
        \(slaves, rand) eId ->
          let (r, rand')  = R.random rand
              g = D.def & GS.env . EN.rand .~ (R.mkStdGen r) & GS.env . EN.slaveEIds .~ slaveEIds
          in ((eId, g):slaves, rand')
      slaveState = Mp.fromList slaves
   in GlobalState slaveEIds clientEIds queues nonemptyQueues slaveState rand'

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
   let msg Sq.:< queue' = Sq.viewl $ g ^. queues . ix fromEId . ix toEId
       queues' = g ^. queues & ix fromEId %~ ix toEId .~ queue'
       nonemptyQueues' = if (Sq.length queue') == 0
                           then g ^. nonemptyQueues & St.delete (fromEId, toEId)
                           else g ^. nonemptyQueues
       Just state = g ^. slaveState . at toEId
       mpMsg = MH.handleMessage msg
       (_, (msgsO, state')) = runST ((lp3 (GS.multiPaxosInstance, GS.paxosLog, GS.env))
         St..^ MP.handleMultiPaxos fromEId mpMsg) state
       slaveState' = g ^. slaveState & ix toEId .~ state'
       (queues'', nonemptyQueues'') = U.s13 foldl (queues', nonemptyQueues') msgsO $
         \(queues', nonemptyQueues') msgO ->
           case msgO of
             A.Send eIds msg -> U.s13 foldl (queues', nonemptyQueues') eIds $
               \(queues', nonemptyQueues') eId ->
                 addMsg msg (toEId, eId) (queues', nonemptyQueues')
   in g & queues .~ queues''
        & nonemptyQueues .~ nonemptyQueues''
        & slaveState .~ slaveState'

simulateOnce :: GlobalState -> GlobalState
simulateOnce g =
  if (g ^. nonemptyQueues & St.size) == 0
    then g
    else
      let (randQueueIndex, rg') = R.randomR (0, g ^. nonemptyQueues & St.size & (subtract 1)) (g ^. rand)
          g' = g & rand .~ rg'
          queueId = g' ^. nonemptyQueues & St.elemAt randQueueIndex
          g'' = deliverMessage queueId g'
      in g''

simulateN :: Int -> GlobalState -> GlobalState
simulateN n g =
  if (g ^. nonemptyQueues & St.size) == 0 || n == 0
    then g
    else simulateN (n - 1) $ simulateOnce g

simulateAll :: GlobalState -> GlobalState
simulateAll g =
  if (g ^. nonemptyQueues & St.size) == 0
    then g
    else simulateAll $ simulateOnce g

addClientMsg :: Int -> Int -> GlobalState -> GlobalState
addClientMsg slaveId cliengMsgId g =
  let (clientEId, slaveEId) = (mkClientEId 0, mkSlaveEId slaveId)
      msg = M.MultiPaxosMessage $ PM.Insert $ PM.Write ("key " ++ show cliengMsgId) ("value " ++ show cliengMsgId) 1
  in g & lp2 (queues, nonemptyQueues) %~ addMsg msg (clientEId, slaveEId)

test1 :: GlobalState -> GlobalState
test1 g =
  simulateAll . addClientMsg 0 0 $ g

test2 :: GlobalState -> GlobalState
test2 g =
  simulateN 25 . addClientMsg 4 4 .
  simulateN 25 . addClientMsg 3 3 .
  simulateN 25 . addClientMsg 2 2 .
  simulateN 25 . addClientMsg 1 1 .
  simulateN 25 . addClientMsg 0 0 $ g

mergePaxosLog :: GlobalState -> Maybe (Mp.Map PM.IndexT PM.PaxosLogEntry)
mergePaxosLog g =
  let paxosLogs = g ^. slaveState & Mp.toList & map (^. _2 . GS.paxosLog)
  in U.s13 foldl (Just Mp.empty) paxosLogs $
       \mergedLogM paxosLog ->
         case mergedLogM of
           Nothing -> Nothing
           _ ->
             U.s13 Mp.foldlWithKey mergedLogM (paxosLog & PL.plog) $
               \mergedLogM index value ->
                 case mergedLogM of
                   Nothing -> Nothing
                   Just mergedLog ->
                     case mergedLog ^. at index of
                       Nothing -> Just $ mergedLog & at index ?~ value
                       Just curValue -> if value == curValue
                                          then mergedLogM
                                          else Nothing

driveTest :: Int -> (GlobalState -> GlobalState) -> IO ()
driveTest testNum test = do
  let g = createGlobalState 0 5 1
      g' = test g
  case mergePaxosLog g' of
    Just mergedLog -> print $
      "Test " ++ (show testNum) ++ " Passed! " ++ (show $ Mp.size mergedLog) ++ " entries."
    Nothing -> print $
      "Test " ++ (show testNum) ++ " Failed!"
  return ()

testDriver :: IO ()
testDriver = do
  driveTest 1 test1
  driveTest 2 test2

main :: IO ()
main = testDriver
