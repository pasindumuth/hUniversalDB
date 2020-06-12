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

propose :: M.Rnd -> M.Val -> St.State ProposerState M.PaxosMessage
propose rnd val = St.state $ \s@(ProposerState proposals) ->
  let newProposal = Proposal {
        crnd = rnd,
        cval = val,
        promises = []
      }
      newProposals = Mp.insert rnd newProposal proposals
      newS = s { proposals = newProposals }
      prepareMsg = M.Prepare rnd
  in (prepareMsg, newS)

prepare :: M.Rnd -> St.State AcceptorState (Maybe M.PaxosMessage)
prepare crnd = St.state $ \s@(AcceptorState rnd vrnd vval) ->
  if rnd >= crnd
    then (Nothing, s)
    else
      let newS = s { rnd = crnd }
          promiseMsg = M.Promise crnd vrnd vval
      in (Just promiseMsg, newS)

promise :: M.Rnd -> M.Rnd -> M.Val -> St.State Proposal (Maybe M.PaxosMessage)
promise rnd vrnd vval = St.state $ \s@(Proposal crnd cval promises) ->
  let newPromises = (vrnd, vval):promises
      newS = s { promises = newPromises }
  in if (length newPromises) /= 3
    then (Nothing, newS)
    else
      let maxVal = maximum $ map fst newPromises
          newCval = if maxVal == 0
                      then cval
                      else
                        let ((_, val):_) = filter (\(x, _) -> x == maxVal) newPromises
                        in val
      in (Just (M.Accept crnd newCval), newS)

accept :: M.Rnd -> M.Val -> St.State AcceptorState (Maybe M.PaxosMessage)
accept crnd cval = St.state $ \s@(AcceptorState rnd vrnd vval) ->
  if crnd < rnd
    then (Nothing, s)
    else
      let newS = s { rnd = crnd, vrnd = crnd, vval = cval }
      in (Just (M.Learn crnd cval), newS)

learn :: M.Rnd -> M.Val -> St.State LearnerState (Maybe M.Val)
learn lrnd lval = St.state $ \s@(LearnerState learns) ->
  let (val, count) = case Mp.lookup lrnd learns of
                       Just (val, count) -> (val, count + 1)
                       Nothing -> (lval, 1)
      newS = s { learns = (Mp.insert lrnd (val, count) learns) }
  in if count < 3
    then (Nothing, newS)
    else (Just val, newS)
