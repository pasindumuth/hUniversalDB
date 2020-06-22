{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module ClientRequestHandler where

import qualified Data.Default as D
import qualified Data.Map as Mp
import qualified Data.Maybe as Mb
import qualified Data.Sequence as Sq
import qualified Control.Concurrent.MVar as MV
import qualified GHC.Generics as G
import qualified System.Random as R

import qualified Connections as CC
import qualified MultiPaxosInstance as MP
import qualified PaxosLog as PL
import qualified MultiVersionKVStore as MS
import qualified Message as M
import qualified TabletParticipant as TP
import qualified Utils as U
import Lens (makeLenses, (%~), (.~), (^.), (&), (?~), at, ix, (.^.), _1, _2, wrapMaybe, lp2)

data CurrentInsert = CurrentInsert {
  _index :: Int,
  _entry :: M.PaxosLogEntry,
  _retryCount :: Int,
  _clientMessage :: M.ClientRequest,
  _eId :: CC.EndpointId,
  _try :: Int
} deriving (G.Generic, Show)

data GlobalState = GlobalState {
  _tabletParticipant :: TP.TabletParticipant,
  _currentInsert :: Maybe CurrentInsert,
  _requestQueue :: Sq.Seq (CC.EndpointId, M.ClientRequest),
  _tryCount :: Int
} deriving (G.Generic, D.Default, Show)

type Messages = [(CC.EndpointId, M.Message)]

makeLenses ''CurrentInsert
makeLenses ''GlobalState

--clientRequestHandler
--  :: CC.EndpointId
--  -> M.ClientRequest
--  -> GlobalState
--  -> (Maybe M.Retry, GlobalState)
--clientRequestHandler eId clientRequest g =
--  let g' = g & requestQueue %~ (Sq.|> (eId, clientRequest))
--  in if (g' ^. requestQueue & Sq.length) == 1
--    then next g
--    else (Nothing, g')
--
--next :: GlobalState -> (Maybe M.Retry, GlobalState)
--next g =
--  if (g ^. requestQueue & Sq.length) > 1
--    then
--      let (eId, clientRequest) Sq.:< _ = Sq.viewl $ g ^. requestQueue
--      in g & next' eId clientRequest 0
--    else (Nothing, g)
--
--next'
--  :: CC.EndpointId
--  -> M.ClientRequest
--  -> Int
--  -> GlobalState
--  -> (Maybe M.Retry, GlobalState)
--next' eId clientRequest retryCount g =
--  let nextAvailableIndex = g ^. tabletParticipant . TP.multiPaxos . MP.paxosLog & PL.nextAvailableIndex
--      pLEntry = createPLEntry clientRequest
--      try = g ^. tryCount
--      g' = g & currentInsert .~ Just (CurrentInsert nextAvailableIndex pLEntry retryCount clientRequest eId try)
--  in (Just $ M.Retry try, g')

createPLEntry :: M.ClientRequest -> M.PaxosLogEntry
createPLEntry clientRequest =
  case clientRequest of
    M.CRead key timestamp -> M.Read key timestamp
    M.CWrite key value timestamp -> M.Write key value timestamp

