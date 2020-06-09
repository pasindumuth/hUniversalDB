{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Paxos where

import qualified Data.Binary as B
import qualified Data.Default as D
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified GHC.Generics as G
import qualified Data.ByteString as BS
import qualified Control.Monad.State as St

type Rnd = Int
type Val = BS.ByteString

instance D.Default BS.ByteString where
  def = BS.pack []

data Proposal = Proposal {
  crnd :: Rnd,
  cval :: Val,
  promises :: S.Set (Rnd, Val)
}

data ProposerState = ProposerState {
  proposals :: M.Map Rnd Proposal
} deriving (G.Generic, D.Default) -- Both needed for PaxosInstance

data AcceptorState = AcceptorState {
  rnd :: Rnd,
  vrnd :: Rnd,
  vval :: Val
} deriving (G.Generic, D.Default) -- Both needed for PaxosInstance

data LearnerState = LearnerState {
  learns :: M.Map Rnd (Val, Int)
} deriving (G.Generic, D.Default) -- Both needed for PaxosInstance

data PaxosInstance = PaxosInstance {
  proposerState :: ProposerState,
  acceptorState :: AcceptorState,
  learnerState :: LearnerState
} deriving (G.Generic, D.Default)

data PaxosMessage =
  Propose { crnd :: Rnd, cval :: Val } |
  Prepare { crnd :: Rnd} |
  Promise { crnd :: Rnd, vrnd :: Rnd, vval :: Val } |
  Accept { crnd :: Rnd, cval :: Val } |
  Learn { lrnd :: Rnd, lval :: Val }
  deriving (G.Generic, B.Binary)

propose :: Rnd -> Val -> St.State ProposerState PaxosMessage
propose rnd val = St.state $ \s@(ProposerState proposals) ->
  let newProposal = Proposal {
        crnd = rnd,
        cval = val,
        promises = S.empty
      }
      newProposals = M.insert rnd newProposal proposals
      newS = s { proposals = newProposals }
      prepareMsg = Prepare rnd
  in (prepareMsg, newS)

prepare :: Rnd -> St.State AcceptorState (Maybe PaxosMessage)
prepare crnd = St.state $ \s@(AcceptorState rnd vrnd vval) ->
  if rnd >= crnd
    then (Nothing, s)
    else
      let newS = s { rnd = crnd }
          promiseMsg = Promise crnd vrnd vval
      in (Just promiseMsg, newS)

promise :: Rnd -> Rnd -> Val -> St.State Proposal (Maybe PaxosMessage)
promise rnd vrnd vval = St.state $ \s@(Proposal crnd cval promises) ->
  let newPromises = S.insert (vrnd, vval) promises
      newS = s { promises = newPromises }
  in if (S.size newPromises) < 3
    then (Nothing, newS)
    else
      let (maxVal, _) = S.findMax newPromises
          newCval = if maxVal == 0
                      then cval
                      else
                        let v = S.filter (\(x, _) -> x == maxVal) newPromises -- all elements of this set should be the same
                        in S.elemAt 0 (S.map snd v)
      in (Just (Accept crnd newCval), newS)

accept :: Rnd -> Val -> St.State AcceptorState (Maybe PaxosMessage)
accept crnd cval = St.state $ \s@(AcceptorState rnd vrnd vval) ->
  if crnd < rnd
    then (Nothing, s)
    else
      let newS = s { rnd = crnd, vrnd = crnd, vval = cval }
      in (Just (Learn crnd cval), newS)

learn :: Rnd -> Val -> St.State LearnerState (Maybe BS.ByteString)
learn lrnd lval = St.state $ \s@(LearnerState learns) ->
  let (val, count) = case M.lookup lrnd learns of
                       Just (val, count) -> (val, count + 1)
                       Nothing -> (lval, lrnd)
      newS = s { learns = (M.insert lrnd (val, count) learns) }
  in if count < 3
    then (Nothing, newS)
    else (Just val, newS)
