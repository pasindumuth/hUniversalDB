module Paxos.MultiPaxosInstance (
  MP.MultiPaxosInstance,
  insertMultiPaxos,
  handleMultiPaxos,
) where

import qualified Data.Default as Df
import qualified System.Random as Rn

import qualified Paxos.Internal_MultiPaxosInstance as MP
import qualified Paxos.PaxosLog as PL
import qualified Paxos.PaxosInstance as PI
import qualified Proto.Actions.Actions as Ac
import qualified Proto.Common as Co
import qualified Proto.Messages as Ms
import qualified Proto.Messages.PaxosMessages as PM
import Infra.Lens
import Infra.State

maxRndIncrease = 1000

getPaxosInstance :: PM.IndexT -> ST MP.MultiPaxosInstance PI.PaxosInstance
getPaxosInstance index = do
  pIM <- getL $ MP.paxosInstances . at index
  case pIM of
    Just paxosInstance -> return paxosInstance
    Nothing -> do
      let paxosInstance = Df.def :: PI.PaxosInstance
      id .^^. MP.paxosInstances . at index ?~ paxosInstance
      return paxosInstance

insertMultiPaxos
  :: [Co.EndpointId]
  -> PM.PaxosLogEntry
  -> (PM.MultiPaxosMessage -> Ms.Message)
  -> ST (MP.MultiPaxosInstance, PL.PaxosLog, Rn.StdGen) ()
insertMultiPaxos slaveEIds entry msgWrapper = do
  r <- _3 .^^ Rn.randomR (1, maxRndIncrease)
  index <- _2 .^^^ PL.nextAvailableIndex
  paxosInstance <- _1 .^ getPaxosInstance index
  let nextRnd = case PI.maxProposalM paxosInstance of
                 Just (rnd, _) -> rnd + r
                 Nothing -> r
  action <- _1 . MP.paxosInstances . ix index .^* (PI.handlePaxos $ PM.Propose nextRnd entry)
  case action of
    PI.Broadcast paxosMessage -> addA $ Ac.Send slaveEIds $ msgWrapper $ PM.PaxosMessage index paxosMessage

handleMultiPaxos
  :: Co.EndpointId
  -> [Co.EndpointId]
  -> PM.MultiPaxosMessage
  -> (PM.MultiPaxosMessage -> Ms.Message)
  -> ST (MP.MultiPaxosInstance, PL.PaxosLog, Rn.StdGen) ()
handleMultiPaxos fromEId slaveEIds (PM.PaxosMessage index pMsg) msgWrapper = do
  _1 .^ getPaxosInstance index
  action <- _1 . MP.paxosInstances . ix index .^* (PI.handlePaxos pMsg)
  case action of
    PI.Reply paxosMessage -> addA $ Ac.Send [fromEId] $ msgWrapper $ PM.PaxosMessage index paxosMessage
    PI.Broadcast paxosMessage -> addA $ Ac.Send slaveEIds $ msgWrapper $ PM.PaxosMessage index paxosMessage
    PI.Choose chosenValue -> do
      _2 .^^. (PL.insert index chosenValue)
      return ()
    _ -> return ()
