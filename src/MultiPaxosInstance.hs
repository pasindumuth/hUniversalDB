module MultiPaxosInstance (
  MP.MultiPaxosInstance,
  handleMultiPaxos,
  MP.paxosLog -- remove once PaxosLog is moved out of MultiPaxosInstance
) where

import qualified Data.Default as D
import qualified System.Random as R

import qualified Connections as CC
import qualified PaxosInstance as PI
import qualified PaxosLog as PL
import qualified Message as M
import qualified Records.MultiPaxosInstance as MP
import Lens ((%~), (.~), (^.), (&), (?~), at, ix, (.^.), wrapMaybe)

maxRndIncrease = 1000

getPaxosInstance ::  M.IndexT -> MP.MultiPaxosInstance -> (PI.PaxosInstance, MP.MultiPaxosInstance)
getPaxosInstance index m =
  case m ^. MP.paxosInstances . at index of
    Just paxosInstance -> (paxosInstance, m)
    Nothing ->
      let paxosInstance' = D.def :: PI.PaxosInstance
      in (paxosInstance', m & MP.paxosInstances . at index ?~ paxosInstance')

handleMultiPaxos
  :: [CC.EndpointId]
  -> CC.EndpointId
  -> M.MultiPaxosMessage
  -> (MP.MultiPaxosInstance, R.StdGen)
  -> ([(CC.EndpointId, M.MultiPaxosMessage)], (MP.MultiPaxosInstance, R.StdGen))
handleMultiPaxos eIds fromEId msg (m, rg) =
  let (r, rg') = rg & R.randomR (1, maxRndIncrease)
      index = case msg of
                M.Insert _ -> PL.nextAvailableIndex $ m ^. MP.paxosLog
                M.PMessage index _ -> index
      (paxosInstance, m') = m & getPaxosInstance index
      pMsg = case msg of
                M.Insert value -> 
                  let maxProposalM = PI.maxProposalM paxosInstance
                      nextRnd = case maxProposalM of
                                  Just (rnd, _) -> rnd + r
                                  Nothing -> r
                  in M.Propose nextRnd value
                M.PMessage _ value -> value
      (action, m'') = m' .^. MP.paxosInstances . at index $ wrapMaybe $ PI.handlePaxos pMsg
      m''' = case action of
               PI.Choose chosenValue -> m'' & MP.paxosLog %~ (PL.insert index chosenValue)
               _ -> m''
      msgsO = case action of
             PI.Reply paxosMessage -> [(fromEId, M.PMessage index paxosMessage)]
             PI.Broadcast paxosMessage -> flip map eIds $ \e -> (e, M.PMessage index paxosMessage)
             _ -> []
  in (msgsO, (m''', rg'))
