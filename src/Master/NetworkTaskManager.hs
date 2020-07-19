{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Master.NetworkTaskManager (
  NetworkTask(..),
  NetworkTaskManager,
  taskMap,
  performTask
) where

import qualified Data.Default as Df
import qualified Data.Map as Mp
import qualified GHC.Generics as Gn

import qualified Master.SlaveGroupRanges as SGR
import qualified Proto.Actions.MasterActions as MAc
import qualified Proto.Common as Co
import qualified Proto.Messages as Ms
import qualified Proto.Messages.ClientRequests as CRq
import qualified Proto.Messages.TraceMessages as TrM
import Infra.Lens
import Infra.State

data NetworkTask =
  CreateDatabase Co.EndpointId Co.RequestId Co.Timestamp Co.SlaveGroupId |
  DeleteDatabase Co.EndpointId Co.RequestId Co.Timestamp Co.SlaveGroupId
  deriving (Show)

data NetworkTaskManager = NetworkTaskManager {
  _i'taskMap :: Mp.Map Co.UID NetworkTask
} deriving (Gn.Generic, Df.Default, Show)

makeLenses ''NetworkTaskManager

taskMap :: Lens' NetworkTaskManager (Mp.Map Co.UID NetworkTask)
taskMap = i'taskMap

performTask
  :: Co.UID
  -> NetworkTaskManager
  -> SGR.SlaveGroupRanges
  -> Mp.Map Co.SlaveGroupId [Co.EndpointId]
  -> STM () ()
performTask uid taskManager slaveGroupRanges slaveGroupEIds = do
  case Mp.lookup uid (taskManager ^. i'taskMap) of
    Just task ->
      case task of
        CreateDatabase _ _ timestamp slaveGroupId -> handleRangeWrite timestamp slaveGroupId
        DeleteDatabase _ _ timestamp slaveGroupId -> handleRangeWrite timestamp slaveGroupId
    _ -> return ()
  where
    handleRangeWrite timestamp slaveGroupId = do
      case SGR.staticRead slaveGroupId timestamp slaveGroupRanges of
        Just (_, SGR.Changing changingKeySpace) -> do
          -- TODO: we should probably have a different type of message for communication between
          -- the Datamaster and the slaves.
          let request =
                Ms.ClientRequest
                  (CRq.ClientRequest
                    (CRq.Meta (Co.getUid uid))
                    (CRq.RangeWrite (changingKeySpace ^. Co.newKeySpace) timestamp))
              -- TODO: This is so wrong in so many ways. We should be accessing a table indexed by SlaveGroupId,
              -- and then picking a slave that's not dead. Also, we need to know if this master should even
              -- be doing this (since this might be a backup master).
              Just slaveEIds = Mp.lookup slaveGroupId slaveGroupEIds
              (slaveEId:_) = slaveEIds
          addA $ MAc.Send [slaveEId] request
          addA $ MAc.PerformOutput uid 100
        _ -> return ()
