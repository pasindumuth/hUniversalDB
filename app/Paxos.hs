{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Paxos where

import qualified Data.Default as D
import qualified Data.Map.Strict as Mp
import qualified GHC.Generics as G
import qualified Control.Monad.State as St

import qualified Message as M

data Proposal = Proposal {
  crnd :: M.Rnd,
  cval :: M.Val,
  promises :: [(M.Rnd, M.Val)]
} deriving (Show)

data ProposerState = ProposerState {
  proposals :: Mp.Map M.Rnd Proposal
} deriving (G.Generic, D.Default, Show) -- First two needed for PaxosInstance

data AcceptorState = AcceptorState {
  rnd :: M.Rnd,
  vrnd :: M.Rnd,
  vval :: M.Val
} deriving (G.Generic, D.Default, Show) -- First two needed for PaxosInstance

data LearnerState = LearnerState {
  learns :: Mp.Map M.Rnd (M.Val, Int)
} deriving (G.Generic, D.Default, Show) -- First two needed for PaxosInstance

data PaxosInstance = PaxosInstance {
  proposerState :: ProposerState,
  acceptorState :: AcceptorState,
  learnerState :: LearnerState
} deriving (G.Generic, D.Default, Show)

data Action =
  Reply M.PaxosMessage |
  Broadcast M.PaxosMessage |
  Choose M.PaxosLogEntry |
  Stall

propose :: ProposerState -> M.Rnd -> M.Val -> (Action, ProposerState)
propose s@(ProposerState proposals) crnd cval =
  let proposals' = Mp.insert crnd (Proposal crnd cval []) proposals
  in (Broadcast $ M.Prepare crnd, s { proposals = proposals' })

prepare :: AcceptorState -> M.Rnd -> (Action, AcceptorState)
prepare s@(AcceptorState rnd vrnd vval) crnd =
  if rnd >= crnd
    then (Stall, s)
    else (Reply $ M.Promise crnd vrnd vval, s { rnd = crnd })

promise :: Proposal -> M.Rnd -> M.Rnd -> M.Val -> (Action, Proposal)
promise s@(Proposal crnd cval promises) rnd vrnd vval =
  let promises' = (vrnd, vval):promises
      s' = s { promises = promises' }
  in if (length promises') /= 3
    then (Stall, s')
    else
      let maxVal = maximum $ map fst promises'
          cval' = if maxVal == 0
                      then cval
                      else
                        let ((_, cval):_) = filter (\(x, _) -> x == maxVal) promises'
                        in cval
      in (Broadcast $ M.Accept crnd cval', s')

accept :: AcceptorState -> M.Rnd -> M.Val -> (Action, AcceptorState)
accept s@(AcceptorState rnd vrnd vval) crnd cval =
  if crnd < rnd
    then (Stall, s)
    else (Broadcast $ M.Learn crnd cval, s { rnd = crnd, vrnd = crnd, vval = cval })

learn :: LearnerState -> M.Rnd -> M.Val -> (Action, LearnerState)
learn s@(LearnerState learns) lrnd lval =
  let (val, count) = case Mp.lookup lrnd learns of
                       Just (val, count) -> (val, count + 1)
                       _ -> (lval, 1)
      s' = s { learns = (Mp.insert lrnd (val, count) learns) }
  in if count < 3
    then (Stall, s')
    else (Choose val, s')

handlePaxos
  :: PaxosInstance
  -> M.PaxosMessage
  -> (Action, PaxosInstance)
handlePaxos paxosIns@PaxosInstance{..} msg = do
  case msg of
    M.Propose crnd cval ->
      let (action, proposerState') = propose proposerState crnd cval
      in (action, paxosIns{ proposerState = proposerState' })
    M.Prepare crnd ->
      let (action, acceptorState') = prepare acceptorState crnd
      in (action, paxosIns{ acceptorState = acceptorState' })
    M.Promise crnd vrnd vval ->
      let ProposerState proposals = proposerState
          Just proposal = Mp.lookup crnd proposals
          (action, proposal') = promise proposal crnd vrnd vval 
          proposals' = Mp.insert crnd proposal' proposals
          proposerState' = proposerState { proposals = proposals' }
      in (action, paxosIns{ proposerState = proposerState' })
    M.Accept crnd cval ->
      let (action, acceptorState') = accept acceptorState crnd cval
      in (action, paxosIns{ acceptorState = acceptorState' })
    M.Learn lrnd lval ->
      let (action, learnerState') = learn learnerState lrnd lval
      in (action, paxosIns{ learnerState = learnerState' })
