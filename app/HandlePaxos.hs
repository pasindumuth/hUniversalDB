{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module HandlePaxos where

import qualified Data.Map as Mp
import qualified Control.Monad.State as St
import qualified System.Log.Logger as L

import qualified Paxos as P
import qualified Network as N
import qualified Message as M

logM :: String -> IO ()
logM msg = L.logM "paxos" L.DEBUG msg

handlePaxos
  :: (Maybe M.PaxosMessage -> N.EndpointId -> IO ())
  -> (Maybe M.PaxosMessage -> IO ())
  -> P.PaxosInstance
  -> N.EndpointId
  -> M.PaxosMessage
  -> IO P.PaxosInstance
handlePaxos sendMsg broadcastMsg paxosIns@P.PaxosInstance{..} endpointId msg = do
  logM "Handling Paxos"
  case msg of
    M.Propose crnd cval -> do
      logM "Handling Propose"
      let (msg, newS) = St.runState (P.propose crnd cval) proposerState
      broadcastMsg (Just msg)
      return paxosIns{ P.proposerState = newS }
    M.Prepare crnd -> do
      logM "Handling Prepare"
      let (msgM, newS) = St.runState (P.prepare crnd) acceptorState
      sendMsg msgM endpointId
      return paxosIns{ P.acceptorState = newS }
    M.Promise crnd vrnd vval -> do
      logM "Handling Promise"
      let proposals = P.proposals proposerState
          Just proposal = Mp.lookup crnd proposals
          (msgM, newProposal) = St.runState (P.promise crnd vrnd vval) proposal
          newProposals = Mp.insert crnd newProposal proposals
          newS = proposerState { P.proposals = newProposals }
      broadcastMsg msgM
      return paxosIns{ P.proposerState = newS }
    M.Accept crnd cval -> do
      logM "Handling Accept"
      let (msgM, newS) = St.runState (P.accept crnd cval) acceptorState
      broadcastMsg msgM
      return paxosIns{ P.acceptorState = newS }
    M.Learn lrnd lval -> do
      logM "Handling Learn"
      let (learnedVal, newS) = St.runState (P.learn lrnd lval) learnerState
      case learnedVal of
        Just val -> L.logM "paxos" L.INFO "Learned something!"
        _ -> return()
      return paxosIns{ P.learnerState = newS }
