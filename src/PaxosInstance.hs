module PaxosInstance (
  P.PaxosInstance,
  Action (Reply, Broadcast, Choose, Stall),
  handlePaxos,
  maxProposalM,
) where

import qualified Data.Map.Strict as Mp

import qualified Message as M
import qualified Records.PaxosInstance as P
import Lens ((%~), (.~), (^.), (&), at, ix, (.^.), wrapMaybe)

data Action =
  Reply M.PaxosMessage |
  Broadcast M.PaxosMessage |
  Choose M.PaxosLogEntry |
  Stall

propose :: M.Rnd -> M.Val -> P.ProposerState -> (Action, P.ProposerState)
propose crnd cval s =
  let s' = s & P.proposals %~ (Mp.insert crnd (P.Proposal crnd cval []))
  in (Broadcast $ M.Prepare crnd, s')

prepare :: M.Rnd -> P.AcceptorState -> (Action, P.AcceptorState)
prepare crnd s =
  if s ^. P.rnd >= crnd
    then (Stall, s)
    else (Reply $ M.Promise crnd (s ^. P.vrnd) (s ^. P.vval), s & P.rnd .~ crnd)

promise :: M.Rnd -> M.Rnd -> M.Val -> P.Proposal -> (Action, P.Proposal)
promise rnd vrnd vval s =
  let promises' = (vrnd, vval):(s ^. P.promises)
      s' = s & P.promises .~ promises'
  in if (length promises') /= 3
    then (Stall, s')
    else
      let maxVal = maximum $ map fst promises'
          cval' = if maxVal == 0
                      then s ^. P.cval
                      else
                        let ((_, cval):_) = filter (\(x, _) -> x == maxVal) promises'
                        in cval
      in (Broadcast $ M.Accept (s ^. P.crnd) cval', s')

accept :: M.Rnd -> M.Val -> P.AcceptorState -> (Action, P.AcceptorState)
accept crnd cval s =
  if crnd < s ^. P.rnd
    then (Stall, s)
    else (Broadcast $ M.Learn crnd cval, P.AcceptorState crnd crnd cval)

learn :: M.Rnd -> M.Val -> P.LearnerState -> (Action, P.LearnerState)
learn lrnd lval s =
  let count = case s ^. P.learns & Mp.lookup lrnd of
                Just (_, count) -> count + 1
                _ -> 1
      s' = s & P.learns %~ Mp.insert lrnd (lval, count)
  in if count < 3
    then (Stall, s')
    else (Choose lval, s')

handlePaxos
  :: M.PaxosMessage
  -> P.PaxosInstance
  -> (Action, P.PaxosInstance)
handlePaxos msg p = do
  case msg of
    M.Propose crnd cval -> p .^. P.proposerState $ propose crnd cval
    M.Prepare crnd -> p .^. P.acceptorState $ prepare crnd
    M.Promise crnd vrnd vval -> p .^. P.proposerState . P.proposals . at crnd $ wrapMaybe $ promise crnd vrnd vval
    M.Accept crnd cval -> p .^. P.acceptorState $ accept crnd cval
    M.Learn lrnd lval -> p .^. P.learnerState $ learn lrnd lval

maxProposalM :: P.PaxosInstance -> Maybe (M.Rnd, P.Proposal)
maxProposalM paxosInstance = paxosInstance ^. P.proposerState . P.proposals & Mp.lookupMax
