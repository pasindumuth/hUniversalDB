{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module HandlePaxos where

import qualified Data.Map as Mp
import qualified Control.Monad.State as St

import qualified Paxos as P
import qualified Network as N
import qualified Message as M
import qualified Logging as L

handlePaxos
  :: (Maybe M.PaxosMessage -> N.EndpointId -> IO ())
  -> (Maybe M.PaxosMessage -> IO ())
  -> P.PaxosInstance
  -> N.EndpointId
  -> M.PaxosMessage
  -> IO (P.PaxosInstance, Maybe M.PaxosLogEntry)
handlePaxos sendMsg broadcastMsg paxosIns@P.PaxosInstance{..} endpointId msg = do
  L.debugM L.paxos $ "Handling Paxos"
  case msg of
    M.Propose crnd cval -> do
      L.debugM L.paxos $ "Handling Propose " ++ show msg
      let (msg, newS) = St.runState (P.propose crnd cval) proposerState
      broadcastMsg (Just msg)
      return (paxosIns{ P.proposerState = newS }, Nothing)
    M.Prepare crnd -> do
      L.debugM L.paxos $ "Handling Prepare " ++ show msg
      let (msgM, newS) = St.runState (P.prepare crnd) acceptorState
      sendMsg msgM endpointId
      return (paxosIns{ P.acceptorState = newS }, Nothing)
    M.Promise crnd vrnd vval -> do
      L.debugM L.paxos $ "Handling Promise " ++ show msg
      let proposals = P.proposals proposerState
          Just proposal = Mp.lookup crnd proposals
          (msgM, newProposal) = St.runState (P.promise crnd vrnd vval) proposal
          newProposals = Mp.insert crnd newProposal proposals
          newS = proposerState { P.proposals = newProposals }
      broadcastMsg msgM
      return (paxosIns{ P.proposerState = newS }, Nothing)
    M.Accept crnd cval -> do
      L.debugM L.paxos $ "Handling Accept " ++ show msg
      let (msgM, newS) = St.runState (P.accept crnd cval) acceptorState
      broadcastMsg msgM
      return (paxosIns{ P.acceptorState = newS }, Nothing)
    M.Learn lrnd lval -> do
      L.debugM L.paxos $ "Handling Learn " ++ show msg
      let (learnedVal, newS) = St.runState (P.learn lrnd lval) learnerState
      return (paxosIns{ P.learnerState = newS }, learnedVal)
