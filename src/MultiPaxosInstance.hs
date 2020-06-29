module MultiPaxosInstance (
  MP.MultiPaxosInstance,
  handleMultiPaxos,
) where

import qualified Data.Default as Df
import qualified System.Random as Rn

import qualified PaxosInstance as PI
import qualified PaxosLog as PL
import qualified Proto.Actions.Actions as Ac
import qualified Proto.Common as Co
import qualified Records.MultiPaxosInstance as MP
import qualified Proto.Messages.PaxosMessages as PM
import qualified Proto.Messages as Ms
import qualified Records.Env as En
import Lens
import State

maxRndIncrease = 1000

getPaxosInstance ::  PM.IndexT -> ST MP.MultiPaxosInstance PI.PaxosInstance
getPaxosInstance index = do
  pIM <- getL $ MP.paxosInstances . at index
  case pIM of
    Just paxosInstance -> return paxosInstance
    Nothing -> do
      let paxosInstance = Df.def :: PI.PaxosInstance
      id .^^. MP.paxosInstances . at index ?~ paxosInstance
      return paxosInstance

handleMultiPaxos :: Co.EndpointId
  -> PM.MultiPaxosMessage
  -> ST (MP.MultiPaxosInstance, PL.PaxosLog, En.Env) ()
handleMultiPaxos fromEId msg = do
  r <- _3.En.rand .^^ Rn.randomR (1, maxRndIncrease)
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
  fromEIds <- getL $ _3 .En.slaveEIds
  case action of
    PI.Reply paxosMessage -> addA $ Ac.Send [fromEId] $ Ms.MultiPaxosMessage $ PM.PaxosMessage index paxosMessage
    PI.Broadcast paxosMessage -> addA $ Ac.Send fromEIds $ Ms.MultiPaxosMessage $ PM.PaxosMessage index paxosMessage
    _ -> return ()
