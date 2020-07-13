{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Master.DerivedState (
  DerivedState,
  handleDerivedState,
  slaveGroupRanges,
  taskMap
) where

import qualified Control.Monad as Mo
import qualified Data.Default as Df
import qualified Data.Map as Mp
import qualified GHC.Generics as Gn

import qualified Infra.Utils as U
import qualified Paxos.PaxosLog as PL
import qualified Proto.Common as Co
import qualified Proto.Messages.PaxosMessages as PM
import qualified Proto.Messages.TraceMessages as TrM
import qualified Master.SlaveGroupRanges as SGR
import qualified Master.NetworkTask as NT
import Infra.Lens
import Infra.State

data DerivedState = DerivedState {
  _i'slaveGroupRanges :: SGR.SlaveGroupRanges,
  _i'taskMap :: Mp.Map Co.UID NT.NetworkTask
} deriving (Gn.Generic, Df.Default, Show)

makeLenses ''DerivedState

slaveGroupRanges :: Lens' DerivedState SGR.SlaveGroupRanges
slaveGroupRanges = i'slaveGroupRanges

taskMap :: Lens' DerivedState (Mp.Map Co.UID NT.NetworkTask)
taskMap = i'taskMap

handleDerivedState
  :: Co.PaxosId
  -> PL.PaxosLog
  -> PL.PaxosLog
  -> ST DerivedState ()
handleDerivedState paxosId pl pl' = do
  Mo.forM_ (PL.newlyAddedEntries pl pl') $ \(index, plEntry) -> do
      trace $ TrM.PaxosInsertion paxosId index plEntry
      case plEntry of
        PM.Master entry ->
          case entry of
            PM.CreateDatabase requestId databaseId tableId timestamp -> do
              let keySpaceRange = Co.KeySpaceRange databaseId tableId
              lat <- i'slaveGroupRanges .^^^ SGR.staticReadLat
              latestValues <-  i'slaveGroupRanges .^^^ SGR.staticReadAll lat
              -- First, we check if the keySpaceRange exists in a
              -- KeySpace in the SlaveGroupRanges.
              let exists =
                    U.s31 Mp.foldlWithKey Nothing latestValues $ \exists slaveGroupId value ->
                      case exists of
                        Just slaveGroupId -> Just slaveGroupId
                        Nothing -> do
                          case value of
                            Just (_, SGR.Old keySpace) | elem keySpaceRange keySpace ->
                              Just slaveGroupId
                            _ -> Nothing
              case exists of
                Just slaveGroupId -> do
                  i'slaveGroupRanges .^^ SGR.read slaveGroupId timestamp
                  return ()
                Nothing -> do
                  -- If the keySpaceRange doesn't exist, then we know that it should be possible
                  -- to try creating the keySpaceRange. So, we look for a free SlaveGroupRange
                  -- (which we know must exist because of the tryHandling of the Task that inserted
                  -- the CreateDatabase PL entry) and then write a NewKeySpace to it.
                  let freeGroupM =
                        U.s31 Mp.foldlWithKey Nothing latestValues $ \freeGroupM slaveGroupId value ->
                          case freeGroupM of
                            Just freeGroup -> Just freeGroup
                            Nothing ->
                              case value of
                                Just (_, SGR.Old keySpace) -> Just (slaveGroupId, keySpace)
                                Nothing -> Just (slaveGroupId, [])
                                _ -> Nothing
                  case freeGroupM of
                    Just (slaveGroupId, keySpace) -> do
                      i'slaveGroupRanges .^^ SGR.write timestamp (Mp.fromList [(slaveGroupId, (keySpaceRange:keySpace))])
                      return ()
                    Nothing -> U.caseError
        _ -> U.caseError
