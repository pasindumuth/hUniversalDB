module PaxosInstance (
  P.PaxosInstance,
  Action (Reply, Broadcast, Choose, Stall),
  handlePaxos,
  maxProposalM,
) where

import qualified Data.Map.Strict as Mp

import qualified Records.Messages.PaxosMessages as M
import qualified Records.PaxosInstance as P
import Lens ((%~), (.~), (^.), (&), at, ix)
import State (ST, runST, addA, updateS, get, getId, wrapMaybe, (.^), (.^^), (.^^.))

data Action =
  Reply M.PaxosMessage |
  Broadcast M.PaxosMessage |
  Choose M.PaxosLogEntry |
  Stall

propose :: M.Rnd -> M.Val -> ST P.ProposerState Action
propose crnd cval = do
  updateS $ P.proposals %~ (Mp.insert crnd (P.Proposal crnd cval []))
  return $ Broadcast $ M.Prepare crnd

prepare :: M.Rnd -> ST P.AcceptorState Action
prepare crnd = do
  s <- getId
  if s ^. P.rnd >= crnd
    then return Stall
    else do
      updateS $ P.rnd .~ crnd
      return $ Reply $ M.Promise crnd (s ^. P.vrnd) (s ^. P.vval)

promise :: M.Rnd -> M.Rnd -> M.Val -> ST P.Proposal Action
promise rnd vrnd vval = do
  promises' <- P.promises .^^. ((vrnd, vval):)
  s <- getId
  if (length promises') /= 3
    then return Stall
    else do
      let maxVal = maximum $ map fst promises'
          cval' = if maxVal == 0
                      then s ^. P.cval
                      else
                        let ((_, cval):_) = filter (\(x, _) -> x == maxVal) promises'
                        in cval
      return $ Broadcast $ M.Accept (s ^. P.crnd) cval'

accept :: M.Rnd -> M.Val -> ST P.AcceptorState Action
accept crnd cval = do
  rnd <- get P.rnd
  if crnd < rnd
    then return Stall
    else do
      updateS $ \_ -> P.AcceptorState crnd crnd cval
      return $ Broadcast $ M.Learn crnd cval

learn :: M.Rnd -> M.Val -> ST P.LearnerState Action
learn lrnd lval = do
  learns <- get P.learns
  let count = case learns & Mp.lookup lrnd of
                Just (_, count) -> count + 1
                _ -> 1
  updateS $ P.learns %~ Mp.insert lrnd (lval, count)
  if count < 3
    then return $ Stall
    else return $ Choose lval

handlePaxos
  :: M.PaxosMessage
  -> ST P.PaxosInstance Action
handlePaxos msg = do
  case msg of
    M.Propose crnd cval -> P.proposerState .^ propose crnd cval
    M.Prepare crnd -> P.acceptorState .^ prepare crnd
    M.Promise crnd vrnd vval -> P.proposerState . P.proposals . at crnd .^ (wrapMaybe $ promise crnd vrnd vval)
    M.Accept crnd cval -> P.acceptorState .^ accept crnd cval
    M.Learn lrnd lval -> P.learnerState .^ learn lrnd lval

maxProposalM :: P.PaxosInstance -> Maybe (M.Rnd, P.Proposal)
maxProposalM paxosInstance = paxosInstance ^. P.proposerState . P.proposals & Mp.lookupMax
