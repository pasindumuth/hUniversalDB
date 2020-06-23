module MultiPaxosInstance (
  MP.MultiPaxosInstance,
  handleMultiPaxos,
) where

import qualified Data.Default as D
import qualified System.Random as R

import qualified Connections as CC
import qualified PaxosInstance as PI
import qualified PaxosLog as PL
import qualified Message as M
import qualified Records.MultiPaxosInstance as MP
import qualified Records.Env as E
import Lens ((%~), (.~), (^.), (&), (.^.), (?~), at, ix, wrapMaybe, _1, _2, _3)
import State (ST, runST, addA, updateS, get)
import State as St

maxRndIncrease = 1000

getPaxosInstance ::  M.IndexT -> ST MP.MultiPaxosInstance PI.PaxosInstance
getPaxosInstance index = do
  pIM <- get $ MP.paxosInstances . at index
  case pIM of
    Just paxosInstance -> return paxosInstance
    Nothing -> do
      let paxosInstance = D.def :: PI.PaxosInstance
      updateS $ MP.paxosInstances . at index ?~ paxosInstance
      return paxosInstance

handleMultiPaxos
  :: [CC.EndpointId]
  -> CC.EndpointId
  -> M.MultiPaxosMessage
  -> (MP.MultiPaxosInstance, PL.PaxosLog, R.StdGen)
  -> ([(CC.EndpointId, M.MultiPaxosMessage)], (MP.MultiPaxosInstance, PL.PaxosLog, R.StdGen))
handleMultiPaxos eIds fromEId msg (m, paxosLog, rg) =
  let (r, rg') = rg & R.randomR (1, maxRndIncrease)
      index = case msg of
                M.Insert _ -> PL.nextAvailableIndex $ paxosLog
                M.PaxosMessage index _ -> index
      (paxosInstance, (_, m')) = runST (getPaxosInstance index) m
      pMsg = case msg of
                M.Insert value ->
                  let maxProposalM = PI.maxProposalM paxosInstance
                      nextRnd = case maxProposalM of
                                  Just (rnd, _) -> rnd + r
                                  Nothing -> r
                  in M.Propose nextRnd value
                M.PaxosMessage _ value -> value
      (action, (_, m'')) = runST (MP.paxosInstances . at index St..^. St.wrapMaybe (PI.handlePaxos pMsg)) m'
      paxosLog' = case action of
                    PI.Choose chosenValue -> paxosLog & (PL.insert index chosenValue)
                    _ -> paxosLog
      msgsO = case action of
             PI.Reply paxosMessage -> [(fromEId, M.PaxosMessage index paxosMessage)]
             PI.Broadcast paxosMessage -> flip map eIds $ \e -> (e, M.PaxosMessage index paxosMessage)
             _ -> []
  in (msgsO, (m'', paxosLog', rg'))
