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
import Control.Lens (makeLenses, (%~), (.~), (^.), (&))

import qualified Connections as CC
import qualified Paxos as P
import qualified PaxosLog as PL
import qualified Message as M

data MultiPaxos = MultiPaxos {
  _paxosLog :: PL.PaxosLog,
  _paxosInstances :: Mp.Map M.IndexT P.PaxosInstance
} deriving (G.Generic, D.Default)

makeLenses ''MultiPaxos

getPaxosInstance :: MultiPaxos -> M.IndexT -> (P.PaxosInstance, MultiPaxos)
getPaxosInstance m index =
  case Mp.lookup index $ m ^. paxosInstances of
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
  let (r, rg') = R.random rg
      (index, pMsg) =
        case msg of
          M.Insert val ->
            let index = PL.nextAvailableIndex $ m ^. paxosLog
            in (index, M.Propose r val)
          M.PMessage index msg -> (index, msg)
      (paxosInstance, m') = getPaxosInstance m index
      (action, paxosInstance') = P.handlePaxos paxosInstance pMsg
      m'' = m' & paxosInstances %~ (Mp.insert index paxosInstance')
      m''' = case action of
               P.Choose chosenValue -> m'' & paxosLog %~ (PL.insert index chosenValue)
               _ -> m''
      msgsO = case action of
             P.Reply paxosMessage -> [(fromEId, M.PMessage index paxosMessage)]
             P.Broadcast paxosMessage -> flip map eIds $ \e -> (e, M.PMessage index paxosMessage)
             _ -> []
  in (msgsO, (m''', rg'))
