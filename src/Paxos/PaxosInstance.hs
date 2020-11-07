{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Paxos.PaxosInstance (
  PaxosInstance,
  Action (Reply, Broadcast, Choose, Stall),
  handlePaxos,
  maxProposalM,
) where

import qualified Data.Default as Df
import qualified Data.Map.Strict as Mp
import qualified GHC.Generics as Gn

import qualified Proto.Messages.PaxosMessages as M
import Infra.Lens
import Infra.State

data Proposal = Proposal {
  _i'crnd :: M.Rnd,
  _i'cval :: M.Val,
  _i'promises :: [(M.Rnd, Maybe M.Val)]
} deriving (Show)

data ProposerState = ProposerState {
  _i'proposals :: Mp.Map M.Rnd Proposal
} deriving (Gn.Generic, Df.Default, Show)

data AcceptorState = AcceptorState {
  _i'rnd :: M.Rnd,
  _i'vrnd :: M.Rnd,
  _i'vval :: Maybe M.Val
} deriving (Gn.Generic, Df.Default, Show)

data LearnerState = LearnerState {
  _i'learns :: Mp.Map M.Rnd (M.Val, Int)
} deriving (Gn.Generic, Df.Default, Show)

data PaxosInstance = PaxosInstance {
  _i'proposerState :: ProposerState,
  _i'acceptorState :: AcceptorState,
  _i'learnerState :: LearnerState
} deriving (Gn.Generic, Df.Default, Show)

makeLenses ''Proposal
makeLenses ''ProposerState
makeLenses ''AcceptorState
makeLenses ''LearnerState
makeLenses ''PaxosInstance

data Action =
  Reply M.PaxosMessage |
  Broadcast M.PaxosMessage |
  Choose M.PaxosLogEntry |
  Stall
  deriving (Gn.Generic, Show)

propose :: M.Rnd -> M.Val -> ST outputActionT traceMessageT ProposerState Action
propose crnd cval = do
  id .^^. i'proposals %~ (Mp.insert crnd (Proposal crnd cval []))
  return $ Broadcast $ M.Prepare crnd

prepare :: M.Rnd -> ST outputActionT traceMessageT AcceptorState Action
prepare crnd = do
  s <- getL id
  if s ^. i'rnd >= crnd
    then return Stall
    else do
      id .^^. i'rnd .~ crnd
      return $ Reply $ M.Promise crnd (s ^. i'vrnd) (s ^. i'vval)

promise :: M.Rnd -> M.Rnd -> Maybe M.Val -> ST outputActionT traceMessageT Proposal Action
promise rnd vrnd vval = do
  promises' <- i'promises .^^. ((vrnd, vval):)
  s <- getL id
  if (length promises') /= 3
    then return Stall
    else do
      let maxVal = maximum $ map fst promises'
          cval' = if maxVal == 0
                      then s ^. i'cval
                      else
                        let ((_, Just cval):_) = filter (\(x, _) -> x == maxVal) promises'
                        in cval
      return $ Broadcast $ M.Accept (s ^. i'crnd) cval'

accept :: M.Rnd -> M.Val -> ST outputActionT traceMessageT AcceptorState Action
accept crnd cval = do
  rnd <- getL i'rnd
  if crnd < rnd
    then return Stall
    else do
      id .^^. \_ -> AcceptorState crnd crnd (Just cval)
      return $ Broadcast $ M.Learn crnd cval

learn :: M.Rnd -> M.Val -> ST outputActionT traceMessageT LearnerState Action
learn lrnd lval = do
  learns <- getL i'learns
  let count = case learns & Mp.lookup lrnd of
                Just (_, count) -> count + 1
                _ -> 1
  id .^^. i'learns %~ Mp.insert lrnd (lval, count)
  if count < 3
    then return $ Stall
    else return $ Choose lval

handlePaxos
  :: M.PaxosMessage
  -> ST outputActionT traceMessageT PaxosInstance Action
handlePaxos msg = do
  case msg of
    M.Propose crnd cval -> i'proposerState .^ propose crnd cval
    M.Prepare crnd -> i'acceptorState .^ prepare crnd
    M.Promise crnd vrnd vval -> i'proposerState . i'proposals . ix crnd .^* promise crnd vrnd vval
    M.Accept crnd cval -> i'acceptorState .^ accept crnd cval
    M.Learn lrnd lval -> i'learnerState .^ learn lrnd lval

maxProposalM :: PaxosInstance -> Maybe (M.Rnd, Proposal)
maxProposalM paxosInstance = paxosInstance ^. i'proposerState . i'proposals & Mp.lookupMax
