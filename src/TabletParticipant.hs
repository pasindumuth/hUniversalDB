{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module TabletParticipant where

import qualified Data.Default as D
import qualified Data.Map as Mp
import qualified Data.Maybe as Mb
import qualified Control.Concurrent.MVar as MV
import qualified GHC.Generics as G
import qualified System.Random as R

import qualified Connections as CC
import qualified MultiPaxos as MP
import qualified PaxosLog as PL
import qualified MultiVersionKVStore as MS
import qualified Message as M
import qualified Utils as U
import Lens (makeLenses, (%~), (.~), (^.), (&), (?~), at, ix, (.^.), _1, _2, wrapMaybe, lensProduct)

data TabletParticipant = TabletParticipant {
  _multiPaxos :: MP.MultiPaxos,
  _mvkvs :: MS.MultiVersionKVStore
} deriving (G.Generic, D.Default, Show)

data Env = Env {
  _rand :: R.StdGen,
  _slaveEIds :: [CC.EndpointId]
} deriving (Show)

makeLenses ''TabletParticipant
makeLenses ''Env

-- Maintains the derived state.
-- TODO: here, we don't use the return-value of kvstore read. Do that somewhere
handleTabletParticipant
  :: CC.EndpointId
  -> M.MultiPaxosMessage
  -> (TabletParticipant, Env)
  -> ([(CC.EndpointId, M.MultiPaxosMessage)], (TabletParticipant, Env))
handleTabletParticipant fromEId msg (tp, env) =
  let (msgsO, (tp', env')) = (tp, env) .^. (lensProduct (_1 . multiPaxos) (_2 . rand)) $
                               MP.handleMultiPaxos (env ^. slaveEIds) fromEId msg
      entries = PL.newlyAddedEntries (tp ^. multiPaxos . MP.paxosLog) (tp' ^. multiPaxos . MP.paxosLog)
      tp'' = U.s13 foldl tp' entries $ \tp' (_, msg) ->
        case msg of
          M.Read key timestamp -> (tp' .^. mvkvs $ MS.read key timestamp) ^. _2
          M.Write key value timestamp -> (tp' .^. mvkvs $ MS.write key value timestamp) ^. _2
  in (msgsO, (tp'', env'))
