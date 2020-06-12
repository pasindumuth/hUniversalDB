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
import qualified System.Log.Logger as L

import qualified Paxos as P
import qualified Network as N
import qualified PaxosLog as PL
import qualified HandlePaxos as HP
import qualified Message as M

data MultiPaxos = MultiPaxos {
  paxosLog :: PL.PaxosLog,
  paxosInstances :: Mp.Map M.IndexT P.PaxosInstance
} deriving (G.Generic, D.Default)

getInstance :: MultiPaxos -> M.IndexT -> (P.PaxosInstance, MultiPaxos)
getInstance multiPaxos@MultiPaxos{..} index =
  case Mp.lookup index paxosInstances of
    Just paxosIns -> (paxosIns, multiPaxos)
    Nothing ->
      let newPaxosIns = D.def :: P.PaxosInstance
      in (newPaxosIns, multiPaxos { paxosInstances = Mp.insert index newPaxosIns paxosInstances })

updateInstance :: MultiPaxos -> M.IndexT -> P.PaxosInstance -> MultiPaxos
updateInstance multiPaxos@MultiPaxos{..} index paxosInstance =
  multiPaxos { paxosInstances = Mp.insert index paxosInstance paxosInstances }

-- Takes results of Paxos computation and sends the message
sendPaxosMessage ::  MV.MVar N.Connections -> M.IndexT ->(Maybe M.PaxosMessage) -> N.EndpointId -> IO ()
sendPaxosMessage connM index msgM endpointId = do
  case msgM of
    Just msg -> do
      let wrappedMsg = M.MMessage $ M.PMessage index msg
      conn <- MV.readMVar connM
      case Mp.lookup endpointId conn of
        Just chan -> do
          C.writeChan chan wrappedMsg
        _ -> return ()
    _ -> return ()

broadcastPaxosMessage ::  MV.MVar N.Connections -> M.IndexT ->(Maybe M.PaxosMessage) -> IO ()
broadcastPaxosMessage connM index msgM = do
  case msgM of
    Just msg -> do
      let wrappedMsg = M.MMessage $ M.PMessage index msg
      conn <- MV.readMVar connM
      Mo.forM_ (Mp.toList conn) $ \(_, v) -> C.writeChan v wrappedMsg
    _ -> return ()

handleMultiPaxos :: C.Chan (N.EndpointId, M.MultiPaxosMessage) -> MV.MVar N.Connections -> IO ()
handleMultiPaxos chan connM = do
  handlePaxosMessage D.def
  where
    handlePaxosMessage :: MultiPaxos -> IO ()
    handlePaxosMessage multiPaxos@MultiPaxos{..} = do
      (endpointId, msg) <- C.readChan chan
      let (index, pMsg) = case msg of
                            M.Insert val ->
                              let index = PL.nextAvailableIndex paxosLog
                              in (index, (M.Propose index val))
                            M.PMessage index msg -> (index, msg)
          (newInstance, newMultiPaxos) = getInstance multiPaxos index
          sendMsg = (sendPaxosMessage connM index)
          broadcastMsg = (broadcastPaxosMessage connM index)
      updatedIns <- HP.handlePaxos sendMsg broadcastMsg newInstance endpointId pMsg
      handlePaxosMessage $ updateInstance newMultiPaxos index updatedIns
