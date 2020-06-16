{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Paxos where

import qualified Data.Default as D
import qualified Data.Map.Strict as Mp
import qualified GHC.Generics as G
import qualified Control.Monad.State as St
import Control.Lens (makeLenses, (%~), (.~), (^.), (&))

import qualified Message as M

data Proposal = Proposal {
  _crnd :: M.Rnd,
  _cval :: M.Val,
  _promises :: [(M.Rnd, M.Val)]
} deriving (Show)

data ProposerState = ProposerState {
  _proposals :: Mp.Map M.Rnd Proposal
} deriving (G.Generic, D.Default, Show) -- First two needed for PaxosInstance

data AcceptorState = AcceptorState {
  _rnd :: M.Rnd,
  _vrnd :: M.Rnd,
  _vval :: M.Val
} deriving (G.Generic, D.Default, Show) -- First two needed for PaxosInstance

data LearnerState = LearnerState {
  _learns :: Mp.Map M.Rnd (M.Val, Int)
} deriving (G.Generic, D.Default, Show) -- First two needed for PaxosInstance

data PaxosInstance = PaxosInstance {
  _proposerState :: ProposerState,
  _acceptorState :: AcceptorState,
  _learnerState :: LearnerState
} deriving (G.Generic, D.Default, Show)

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

propose :: M.Rnd -> M.Val -> ProposerState -> (Action, ProposerState)
propose crnd cval s =
  let s' = s & proposals %~ (Mp.insert crnd (Proposal crnd cval []))
  in (Broadcast $ M.Prepare crnd, s')

prepare :: M.Rnd -> AcceptorState -> (Action, AcceptorState)
prepare crnd s =
  if s ^. rnd >= crnd
    then (Stall, s)
    else (Reply $ M.Promise crnd (s ^. vrnd) (s ^. vval), s & rnd .~ crnd)

promise :: M.Rnd -> M.Rnd -> M.Val -> Proposal -> (Action, Proposal)
promise rnd vrnd vval s =
  let promises' = (vrnd, vval):(s ^. promises)
      s' = s & promises .~ promises'
  in if (length promises') /= 3
    then (Stall, s')
    else
      let maxVal = maximum $ map fst promises'
          cval' = if maxVal == 0
                      then s ^. cval
                      else
                        let ((_, cval):_) = filter (\(x, _) -> x == maxVal) promises'
                        in cval
      in (Broadcast $ M.Accept (s ^. crnd) cval', s')

accept :: M.Rnd -> M.Val -> AcceptorState -> (Action, AcceptorState)
accept crnd cval s =
  if crnd < s ^. rnd
    then (Stall, s)
    else (Broadcast $ M.Learn crnd cval, AcceptorState crnd crnd cval)

learn :: M.Rnd -> M.Val -> LearnerState -> (Action, LearnerState)
learn lrnd lval s =
  let count = case s ^. learns & Mp.lookup lrnd of
                Just (_, count) -> count + 1
                _ -> 1
      s' = s & learns %~ Mp.insert lrnd (lval, count)
  in if count < 3
    then (Stall, s')
    else (Choose lval, s')

handlePaxos
  :: M.PaxosMessage
  -> PaxosInstance
  -> (Action, PaxosInstance)
handlePaxos msg p = do
  case msg of
    M.Propose crnd cval ->
      let (action, proposerState') = propose crnd cval (p ^. proposerState)
      in (action, p & proposerState .~ proposerState')
    M.Prepare crnd ->
      let (action, acceptorState') = prepare crnd (p ^. acceptorState)
      in (action, p & acceptorState .~ acceptorState')
    M.Promise crnd vrnd vval ->
      let Just proposal = p ^. proposerState . proposals & Mp.lookup crnd
          (action, proposal') = promise crnd vrnd vval proposal
      in (action, p & proposerState . proposals %~ Mp.insert crnd proposal')
    M.Accept crnd cval ->
      let (action, acceptorState') = accept crnd cval (p ^. acceptorState)
      in (action, p & acceptorState .~ acceptorState')
    M.Learn lrnd lval ->
      let (action, learnerState') = learn lrnd lval (p ^. learnerState)
      in (action, p & learnerState .~ learnerState')
