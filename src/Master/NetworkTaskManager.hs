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
import qualified Proto.Actions.Actions as Ac
import qualified Proto.Common as Co
import qualified Proto.Messages as Ms
import qualified Proto.Messages.ClientRequests as CRq
import qualified Proto.Messages.TraceMessages as TrM
import Infra.Lens
import Infra.State

data NetworkTask =
  CreateDatabase Co.EndpointId Co.RequestId Co.Timestamp Co.SlaveGroupId
  deriving (Gn.Generic, Df.Default, Show)

data NetworkTaskManager = NetworkTaskManager {
  _i'taskMap :: Mp.Map Co.UID NetworkTask
} deriving (Gn.Generic, Df.Default, Show)

makeLenses ''NetworkTaskManager

taskMap :: Lens' NetworkTaskManager (Mp.Map Co.UID NetworkTask)
taskMap = i'taskMap

performTask :: Co.UID -> NetworkTaskManager -> SGR.SlaveGroupRanges -> ST () ()
performTask uid taskManager slaveGroupRanges = do
  case Mp.lookup uid (taskManager ^. i'taskMap) of
    Just task ->
      case task of
        CreateDatabase _ _ timestamp slaveGroupId -> do
          case SGR.staticRead slaveGroupId timestamp slaveGroupRanges of
            Just (_, SGR.Changing changingKeySpace) -> do
              -- TODO: we should probably have a different type of message for communication between
              -- the Datamaster and the slaves.
              let request =
                    Ms.ClientRequest
                      (CRq.ClientRequest
                        (CRq.Meta uid)
                        (CRq.RangeWrite (changingKeySpace ^. Co.newKeySpace) timestamp))
                  -- TODO: This is so wrong in so many ways. We should be accessing a table indexed by SlaveGroupId,
                  -- and then picking a slave that's not dead. Also, we need to know if this master should even
                  -- be doing this (since this might be a backup master).
                  slaveEId = "universal0"
              addA $ Ac.Send [slaveEId] request
              addA $ Ac.PerformOutput uid 100
              -- TODO: we must add the Perform UID Delay Output Action (and Input Action, and handle that Input Action)
            _ -> return ()
    _ -> return ()
