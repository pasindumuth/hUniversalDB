{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module MultiPaxos where

import qualified Data.Default as D
import qualified Data.Map as Mp
import qualified Control.Concurrent as C
import qualified Control.Concurrent.MVar as MV
import qualified Control.Monad as Mo
import qualified GHC.Generics as G
import Control.Lens (makeLenses, (%~), (^.), (&))

import qualified Connections as CC
import qualified Paxos as P
import qualified Network as N
import qualified PaxosLog as PL
import qualified Logging as L
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

-- Takes results of Paxos computation and sends the message
sendPaxosMessage :: MV.MVar CC.Connections -> M.IndexT -> M.PaxosMessage -> CC.EndpointId -> IO ()
sendPaxosMessage connM index msg endpointId = do
  conn <- MV.readMVar connM
  let Just sendFunc = Mp.lookup endpointId conn
  sendFunc $ M.MMessage $ M.PMessage index msg

broadcastPaxosMessage :: MV.MVar CC.Connections -> M.IndexT -> M.PaxosMessage -> IO ()
broadcastPaxosMessage connM index msg = do
  conn <- MV.readMVar connM
  Mo.forM_ (Mp.toList conn) $ \(_, sendFunc) -> sendFunc $ M.MMessage $ M.PMessage index msg

handleMultiPaxos
  :: MultiPaxos
  -> IO (CC.EndpointId, M.MultiPaxosMessage)
  -> MV.MVar CC.Connections
  -> IO MultiPaxos
handleMultiPaxos m getPaxosMsg connM = do
  (endpointId, msg) <- getPaxosMsg
  let (index, pMsg) = case msg of
                        M.Insert val ->
                          let index = PL.nextAvailableIndex $ m ^. paxosLog
                          in (index, M.Propose 10 val) -- TODO pick rounds randomly when retrying
                        M.PMessage index msg -> (index, msg)
      (paxosInstance, m') = getPaxosInstance m index
      (action, paxosInstance') = P.handlePaxos paxosInstance pMsg
      m'' = m' & paxosInstances %~ (Mp.insert index paxosInstance')
  case action of
    P.Reply paxosMessage -> sendPaxosMessage connM index paxosMessage endpointId
    P.Broadcast paxosMessage -> broadcastPaxosMessage connM index paxosMessage
    _ -> return ()
  m''' <- case action of
    P.Choose chosenValue -> do
      L.infoM L.paxos $ "Learned: " ++ show chosenValue
      return $ m'' & paxosLog %~ (PL.insert index chosenValue)
    _ -> return m''
  return m'''

handleMultiPaxosThread :: IO (CC.EndpointId, M.MultiPaxosMessage) -> MV.MVar CC.Connections -> IO ()
handleMultiPaxosThread getPaxosMsg connM = do
  handlePaxosMessage D.def
  where
    handlePaxosMessage :: MultiPaxos -> IO ()
    handlePaxosMessage multiPaxos = do
      newMultiPaxos <- handleMultiPaxos multiPaxos getPaxosMsg connM
      handlePaxosMessage newMultiPaxos
