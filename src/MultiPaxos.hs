{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module MultiPaxos where

import qualified Data.Default as D
import qualified Data.Map as Mp
import qualified Data.Maybe as Mb
import qualified Control.Concurrent.MVar as MV
import qualified GHC.Generics as G
import qualified System.Random as R
import Control.Lens (makeLenses, (%~), (.~), (^.), (&), (?~))
import Control.Lens.At (at, ix)

import qualified Connections as CC
import qualified Paxos as P
import qualified PaxosLog as PL
import qualified Message as M

maxRndIncrease = 1000

data MultiPaxos = MultiPaxos {
  _paxosLog :: PL.PaxosLog,
  _paxosInstances :: Mp.Map M.IndexT P.PaxosInstance
} deriving (G.Generic, D.Default, Show)

makeLenses ''MultiPaxos

getPaxosInstance :: MultiPaxos -> M.IndexT -> (P.PaxosInstance, MultiPaxos)
getPaxosInstance m index =
  case m ^. paxosInstances . at index of
    Just paxosInstance -> (paxosInstance, m)
    Nothing ->
      let paxosInstance' = D.def :: P.PaxosInstance
      in (paxosInstance', m & paxosInstances %~ (Mp.insert index paxosInstance'))

handleMultiPaxos
  :: [CC.EndpointId]
  -> CC.EndpointId
  -> M.MultiPaxosMessage
  -> (MultiPaxos, R.StdGen)
  -> ([(CC.EndpointId, M.MultiPaxosMessage)], (MultiPaxos, R.StdGen))
handleMultiPaxos eIds fromEId msg (m, rg) =
  let (r, rg') = R.randomR (1, maxRndIncrease) rg
      index = case msg of
                M.Insert _ -> PL.nextAvailableIndex $ m ^. paxosLog
                M.PMessage index _ -> index
      (paxosInstance, m') = getPaxosInstance m index
      pMsg = case msg of
                M.Insert value -> 
                  let maxProposalM = paxosInstance ^. P.proposerState . P.proposals & Mp.lookupMax
                      nextRnd = case maxProposalM of
                                  Just (rnd, _) -> rnd + r
                                  Nothing -> r
                  in M.Propose nextRnd value
                M.PMessage _ value -> value
      (action, paxosInstance') = P.handlePaxos pMsg paxosInstance
      m'' = m' & paxosInstances %~ (Mp.insert index paxosInstance')
      m''' = case action of
               P.Choose chosenValue -> m'' & paxosLog %~ (PL.insert index chosenValue)
               _ -> m''
      msgsO = case action of
             P.Reply paxosMessage -> [(fromEId, M.PMessage index paxosMessage)]
             P.Broadcast paxosMessage -> flip map eIds $ \e -> (e, M.PMessage index paxosMessage)
             _ -> []
  in (msgsO, (m''', rg'))
