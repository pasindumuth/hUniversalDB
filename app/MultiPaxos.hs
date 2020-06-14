{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module MultiPaxos where

import qualified Data.Default as D
import qualified Data.Map as Mp
import qualified Control.Concurrent as C
import qualified Control.Concurrent.MVar as MV
import qualified Control.Monad as Mo
import qualified GHC.Generics as G

import qualified Connections as CC
import qualified Paxos as P
import qualified Network as N
import qualified PaxosLog as PL
import qualified Logging as L
import qualified Message as M

data MultiPaxos = MultiPaxos {
  paxosLog :: PL.PaxosLog,
  paxosInstances :: Mp.Map M.IndexT P.PaxosInstance
} deriving (G.Generic, D.Default)

getPaxosInstance :: MultiPaxos -> M.IndexT -> (P.PaxosInstance, MultiPaxos)
getPaxosInstance multiPaxos@MultiPaxos{..} index =
  case Mp.lookup index paxosInstances of
    Just paxosInstance -> (paxosInstance, multiPaxos)
    Nothing ->
      let paxosInstance' = D.def :: P.PaxosInstance
      in (paxosInstance', multiPaxos { paxosInstances = Mp.insert index paxosInstance' paxosInstances })

updateInstance :: MultiPaxos -> M.IndexT -> P.PaxosInstance -> MultiPaxos
updateInstance multiPaxos@MultiPaxos{..} index paxosInstance =
  multiPaxos { paxosInstances = Mp.insert index paxosInstance paxosInstances }

updatePL :: MultiPaxos -> M.IndexT -> M.PaxosLogEntry -> MultiPaxos
updatePL multiPaxos@MultiPaxos{..} index entry =
  multiPaxos { paxosLog = PL.insert paxosLog index entry }

-- Takes results of Paxos computation and sends the message
sendPaxosMessage :: MV.MVar CC.Connections -> M.IndexT -> M.PaxosMessage -> CC.EndpointId -> IO ()
sendPaxosMessage connM index msg endpointId = do
  let wrappedMsg = M.MMessage $ M.PMessage index msg
  conn <- MV.readMVar connM
  let Just sendFunc = Mp.lookup endpointId conn 
  sendFunc wrappedMsg

broadcastPaxosMessage :: MV.MVar CC.Connections -> M.IndexT -> M.PaxosMessage -> IO ()
broadcastPaxosMessage connM index msg = do
  let wrappedMsg = M.MMessage $ M.PMessage index msg
  conn <- MV.readMVar connM
  Mo.forM_ (Mp.toList conn) $ \(_, sendFunc) -> sendFunc wrappedMsg

handleMultiPaxos
  :: MultiPaxos
  -> IO (CC.EndpointId, M.MultiPaxosMessage)
  -> MV.MVar CC.Connections
  -> IO MultiPaxos
handleMultiPaxos multiPaxos@MultiPaxos{..} getPaxosMsg connM = do
  (endpointId, msg) <- getPaxosMsg
  let (index, pMsg) = case msg of
                        M.Insert val ->
                          let index = PL.nextAvailableIndex paxosLog
                          in (index, M.Propose 10 val) -- TODO pick rounds randomly when retrying
                        M.PMessage index msg -> (index, msg)
      (paxosInstance, multiPaxos') = getPaxosInstance multiPaxos index
      (action, paxosInstance') = P.handlePaxos paxosInstance pMsg
      multipaxos'' = updateInstance multiPaxos' index paxosInstance'
  case action of
    P.Reply paxosMessage -> sendPaxosMessage connM index paxosMessage endpointId
    P.Broadcast paxosMessage -> broadcastPaxosMessage connM index paxosMessage
    _ -> return ()
  multiPaxos''' <- case action of
    P.Choose chosenValue -> do
      L.infoM L.paxos $ "Learned: " ++ show chosenValue
      return $ updatePL multipaxos'' index chosenValue
    _ -> return multipaxos''
  return multiPaxos'''

handleMultiPaxosThread :: IO (CC.EndpointId, M.MultiPaxosMessage) -> MV.MVar CC.Connections -> IO ()
handleMultiPaxosThread getPaxosMsg connM = do
  handlePaxosMessage D.def
  where
    handlePaxosMessage :: MultiPaxos -> IO ()
    handlePaxosMessage multiPaxos = do
      newMultiPaxos <- handleMultiPaxos multiPaxos getPaxosMsg connM
      handlePaxosMessage newMultiPaxos
