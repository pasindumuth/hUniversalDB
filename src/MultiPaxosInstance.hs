module MultiPaxosInstance (
  MP.MultiPaxosInstance,
  handleMultiPaxos,
) where

import qualified Data.Default as D
import qualified System.Random as R

import qualified Connections as CC
import qualified PaxosInstance as PI
import qualified PaxosLog as PL
import qualified Records.Actions.Actions as A
import qualified Records.MultiPaxosInstance as MP
import qualified Records.Messages.PaxosMessages as PM
import qualified Records.Messages.Messages as M
import qualified Records.Env as E
import Lens ((?~), at, ix, _1, _2, _3)
import State

maxRndIncrease = 1000

getPaxosInstance ::  PM.IndexT -> ST MP.MultiPaxosInstance PI.PaxosInstance
getPaxosInstance index = do
  pIM <- getL $ MP.paxosInstances . at index
  case pIM of
    Just paxosInstance -> return paxosInstance
    Nothing -> do
      let paxosInstance = D.def :: PI.PaxosInstance
      id .^^. MP.paxosInstances . at index ?~ paxosInstance
      return paxosInstance

handleMultiPaxos :: CC.EndpointId
  -> PM.MultiPaxosMessage
  -> ST (MP.MultiPaxosInstance, PL.PaxosLog, E.Env) ()
handleMultiPaxos fromEId msg = do
  r <- _3.E.rand .^^ R.randomR (1, maxRndIncrease)
  index <- case msg of
             PM.Insert _ -> _2 .^^^ PL.nextAvailableIndex
             PM.PaxosMessage index _ -> return index
  paxosInstance <- _1 .^ getPaxosInstance index
  let pMsg = case msg of
               PM.Insert value ->
                 let maxProposalM = PI.maxProposalM paxosInstance
                     nextRnd = case maxProposalM of
                                 Just (rnd, _) -> rnd + r
                                 Nothing -> r
                 in PM.Propose nextRnd value
               PM.PaxosMessage _ value -> value
  action <- _1 . MP.paxosInstances . ix index .^* (PI.handlePaxos pMsg)
  case action of
    PI.Choose chosenValue -> _2 .^^. (PL.insert index chosenValue)
    _ -> getL _2
  fromEIds <- getL $ _3 .E.slaveEIds
  case action of
    PI.Reply paxosMessage -> addA $ A.Send [fromEId] $ M.MultiPaxosMessage $ PM.PaxosMessage index paxosMessage
    PI.Broadcast paxosMessage -> addA $ A.Send fromEIds $ M.MultiPaxosMessage $ PM.PaxosMessage index paxosMessage
    _ -> return ()
