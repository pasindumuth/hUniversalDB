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

propose :: ProposerState -> M.Rnd -> M.Val -> (Action, ProposerState)
propose s crnd cval =
  let s' = s & proposals %~ (Mp.insert crnd (Proposal crnd cval []))
  in (Broadcast $ M.Prepare crnd, s')

prepare :: AcceptorState -> M.Rnd -> (Action, AcceptorState)
prepare s crnd =
  if s ^. rnd >= crnd
    then (Stall, s)
    else (Reply $ M.Promise crnd (s ^. vrnd) (s ^. vval), s & rnd .~ crnd)

promise :: Proposal -> M.Rnd -> M.Rnd -> M.Val -> (Action, Proposal)
promise s rnd vrnd vval =
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

accept :: AcceptorState -> M.Rnd -> M.Val -> (Action, AcceptorState)
accept s crnd cval =
  if crnd < s ^. rnd
    then (Stall, s)
    else (Broadcast $ M.Learn crnd cval, AcceptorState crnd crnd cval)

learn :: LearnerState -> M.Rnd -> M.Val -> (Action, LearnerState)
learn s lrnd lval =
  let count = case s ^. learns & Mp.lookup lrnd of
                Just (_, count) -> count + 1
                _ -> 1
      s' = s & learns %~ Mp.insert lrnd (lval, count)
  in if count < 3
    then (Stall, s')
    else (Choose lval, s')

handlePaxos
  :: PaxosInstance
  -> M.PaxosMessage
  -> (Action, PaxosInstance)
handlePaxos p msg = do
  case msg of
    M.Propose crnd cval ->
      let (action, proposerState') = propose (p ^. proposerState) crnd cval
      in (action, p & proposerState .~ proposerState')
    M.Prepare crnd ->
      let (action, acceptorState') = prepare (p ^. acceptorState) crnd
      in (action, p & acceptorState .~ acceptorState')
    M.Promise crnd vrnd vval ->
      let Just proposal = p ^. proposerState . proposals & Mp.lookup crnd
          (action, proposal') = promise proposal crnd vrnd vval
      in (action, p & proposerState . proposals %~ Mp.insert crnd proposal')
    M.Accept crnd cval ->
      let (action, acceptorState') = accept (p ^. acceptorState) crnd cval
      in (action, p & acceptorState .~ acceptorState')
    M.Learn lrnd lval ->
      let (action, learnerState') = learn (p ^. learnerState) lrnd lval
      in (action, p & learnerState .~ learnerState')
