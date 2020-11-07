{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Paxos.MultiPaxosInstance (
  MultiPaxosInstance,
  constructor,
  paxosLog,
  paxosId,
  insertMultiPaxos,
  handleMultiPaxos,
) where

import qualified Data.Default as Df
import qualified Data.Map as Mp
import qualified GHC.Generics as Gn
import qualified System.Random as Rn

import qualified Infra.Utils as U
import qualified Paxos.PaxosLog as PL
import qualified Paxos.PaxosInstance as PI
import qualified Proto.Actions.Actions as Ac
import qualified Proto.Common as Co
import qualified Proto.Messages as Ms
import qualified Proto.Messages.PaxosMessages as PM
import Infra.Lens
import Infra.State

maxRndIncrease = 1000

data MultiPaxosInstance = MultiPaxosInstance {
  _i'paxosId :: Co.PaxosId,
  _i'paxosInstances :: Mp.Map PM.IndexT PI.PaxosInstance,
  _i'paxosLog :: PL.PaxosLog
} deriving (Gn.Generic, Show)

makeLenses ''MultiPaxosInstance

constructor :: Co.PaxosId -> MultiPaxosInstance
constructor paxosId = MultiPaxosInstance {
  _i'paxosId = paxosId,
  _i'paxosInstances = Df.def,
  _i'paxosLog = Df.def
}

paxosId :: Lens' MultiPaxosInstance Co.PaxosId
paxosId = i'paxosId

paxosLog :: Lens' MultiPaxosInstance PL.PaxosLog
paxosLog = i'paxosLog

getPaxosInstance
  :: PM.IndexT
  -> ST outputActionT traceMessageT MultiPaxosInstance PI.PaxosInstance
getPaxosInstance index = do
  pIM <- getL $ i'paxosInstances . at index
  case pIM of
    Just paxosInstance -> return paxosInstance
    Nothing -> do
      let paxosInstance = Df.def :: PI.PaxosInstance
      id .^^. i'paxosInstances . at index ?~ paxosInstance
      return paxosInstance

insertMultiPaxos
  :: (Ac.OutputAction outputActionT)
  => [Co.EndpointId]
  -> PM.PaxosLogEntry
  -> (PM.MultiPaxosMessage -> Ms.Message)
  -> ST outputActionT traceMessageT (MultiPaxosInstance, Rn.StdGen) ()
insertMultiPaxos slaveEIds entry msgWrapper = do
  r <- _2 .^^ Rn.randomR (1, maxRndIncrease)
  index <- _1.i'paxosLog .^^^ PL.nextAvailableIndex
  paxosInstance <- _1 .^ getPaxosInstance index
  let nextRnd = case PI.maxProposalM paxosInstance of
                 Just (rnd, _) -> rnd + r
                 Nothing -> r
  action <- _1 . i'paxosInstances . ix index .^* (PI.handlePaxos $ PM.Propose nextRnd entry)
  case action of
    PI.Broadcast paxosMessage -> addA $ Ac.send slaveEIds $ msgWrapper $ PM.PaxosMessage index paxosMessage
    _ -> error $ "Action " ++ (show action) ++ " is not supported."

handleMultiPaxos
  :: (Ac.OutputAction outputActionT)
  => Co.EndpointId
  -> [Co.EndpointId]
  -> PM.MultiPaxosMessage
  -> (PM.MultiPaxosMessage -> Ms.Message)
  -> ST outputActionT traceMessageT (MultiPaxosInstance, Rn.StdGen) ()
handleMultiPaxos fromEId slaveEIds (PM.PaxosMessage index pMsg) msgWrapper = do
  _1 .^ getPaxosInstance index
  action <- _1 . i'paxosInstances . ix index .^* (PI.handlePaxos pMsg)
  case action of
    PI.Reply paxosMessage -> addA $ Ac.send [fromEId] $ msgWrapper $ PM.PaxosMessage index paxosMessage
    PI.Broadcast paxosMessage -> addA $ Ac.send slaveEIds $ msgWrapper $ PM.PaxosMessage index paxosMessage
    PI.Choose chosenValue -> do
      _1.i'paxosLog .^^. (PL.insert index chosenValue)
      return ()
    _ -> return ()
