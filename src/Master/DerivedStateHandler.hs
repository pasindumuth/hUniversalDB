module Master.DerivedStateHandler (
  handleDerivedState,
  findFreeGroupM,
  rangeExistsStrict,
  rangeExists,
  rangeMaybeExists
) where

import qualified Control.Monad as Mo
import qualified Data.Map as Mp
import qualified Data.List as Li

import qualified Infra.Utils as U
import qualified Master.DerivedState as DS
import qualified Master.SlaveGroupRanges as SGR
import qualified Master.NetworkTaskManager as NTM
import qualified Paxos.PaxosLog as PL
import qualified Proto.Common as Co
import qualified Proto.Messages.PaxosMessages as PM
import qualified Proto.Messages.TraceMessages as TrM
import Infra.Lens
import Infra.State

findFreeGroupM
  :: Mp.Map Co.SlaveGroupId (Maybe (Co.Timestamp, SGR.Value))
  -> Maybe (Co.SlaveGroupId, Co.KeySpace)
findFreeGroupM latestValues =
  U.s31 Mp.foldlWithKey Nothing latestValues $ \freeGroupM slaveGroupId value ->
    case freeGroupM of
      Just _ -> freeGroupM
      Nothing ->
        case value of
          Just (_, SGR.Old keySpace) -> Just (slaveGroupId, keySpace)
          Nothing -> Just (slaveGroupId, [])
          _ -> Nothing

rangeExistsStrict
  :: Co.KeySpaceRange
  -> Mp.Map Co.SlaveGroupId (Maybe (Co.Timestamp, SGR.Value))
  -> Maybe (Co.SlaveGroupId, Co.KeySpace)
rangeExistsStrict keySpaceRange latestValues =
  U.s31 Mp.foldlWithKey Nothing latestValues $ \exists slaveGroupId value ->
    case exists of
      Just _ -> exists
      Nothing -> do
        case value of
          Just (_, SGR.Old keySpace) | elem keySpaceRange keySpace ->
            Just (slaveGroupId, keySpace)
          _ -> Nothing

rangeExists
  :: Co.KeySpaceRange
  -> Mp.Map Co.SlaveGroupId (Maybe (Co.Timestamp, SGR.Value))
  -> Maybe Co.SlaveGroupId
rangeExists keySpaceRange latestValues =
  U.s31 Mp.foldlWithKey Nothing latestValues $ \exists slaveGroupId value ->
    case exists of
      Just _ -> exists
      Nothing -> do
        case value of
          Just (_, SGR.Old keySpace) | elem keySpaceRange keySpace -> Just slaveGroupId
          Just (_, SGR.Changing changingKeySpace) |
            elem keySpaceRange (changingKeySpace ^. Co.oldKeySpace) &&
            elem keySpaceRange (changingKeySpace ^. Co.newKeySpace)
           -> Just slaveGroupId
          _ -> Nothing

rangeMaybeExists
  :: Co.KeySpaceRange
  -> Mp.Map Co.SlaveGroupId (Maybe (Co.Timestamp, SGR.Value))
  -> Bool
rangeMaybeExists keySpaceRange latestValues =
  U.s31 Mp.foldl False latestValues $ \maybeExists value ->
    if maybeExists
      then maybeExists
      else
        case value of
          Just (_, SGR.Changing changingKeySpace) ->
            elem keySpaceRange (changingKeySpace ^. Co.oldKeySpace) ||
            elem keySpaceRange (changingKeySpace ^. Co.newKeySpace)
          _ -> False

handleDerivedState
  :: Co.PaxosId
  -> PL.PaxosLog
  -> PL.PaxosLog
  -> STM DS.DerivedState ()
handleDerivedState paxosId pl pl' = do
  Mo.forM_ (PL.newlyAddedEntries pl pl') $ \(index, plEntry) -> do
      trace $ TrM.PaxosInsertion paxosId index plEntry
      case plEntry of
        PM.Master entry ->
          case entry of
            PM.CreateDatabase requestId databaseId tableId timestamp eId uid -> do
              let keySpaceRange = Co.KeySpaceRange databaseId tableId
              lat <- DS.slaveGroupRanges .^^^ SGR.staticReadLat
              latestValues <-  DS.slaveGroupRanges .^^^ SGR.staticReadAll lat
              -- First, we check if the keySpaceRange exists in a
              -- KeySpace in the SlaveGroupRanges.
              case rangeExists keySpaceRange latestValues of
                Just slaveGroupId -> do
                  DS.slaveGroupRanges .^^ SGR.read slaveGroupId timestamp
                  return ()
                Nothing -> do
                  -- If the keySpaceRange doesn't exist, then we know that it should be possible
                  -- to try creating the keySpaceRange. So, we look for a free SlaveGroupRange
                  -- (which we know must exist because of the tryHandling of the Task that inserted
                  -- the CreateDatabase PL entry) and then write a NewKeySpace to it. We also set
                  -- up the NetworkTask.
                  case findFreeGroupM latestValues of
                    Just (slaveGroupId, keySpace) -> do
                      DS.slaveGroupRanges .^^ SGR.write timestamp (Mp.fromList [(slaveGroupId, (keySpaceRange:keySpace))])
                      DS.networkTaskManager . NTM.taskMap .^^. Mp.insert uid (NTM.CreateDatabase eId requestId timestamp slaveGroupId)
                      return ()
                    Nothing -> U.caseError
            PM.DeleteDatabase requestId databaseId tableId timestamp eId uid -> do
              let keySpaceRange = Co.KeySpaceRange databaseId tableId
              lat <- DS.slaveGroupRanges .^^^ SGR.staticReadLat
              latestValues <-  DS.slaveGroupRanges .^^^ SGR.staticReadAll lat
              -- First, we check if the keySpaceRange exists in a
              -- KeySpace in the SlaveGroupRanges.
              case rangeExistsStrict keySpaceRange latestValues of
                Just (slaveGroupId, keySpace) -> do
                  DS.slaveGroupRanges .^^ SGR.write timestamp (Mp.fromList [(slaveGroupId, Li.delete keySpaceRange keySpace)])
                  DS.networkTaskManager . NTM.taskMap .^^. Mp.insert uid (NTM.DeleteDatabase eId requestId timestamp slaveGroupId)
                  return ()
                Nothing -> do
                  -- If the keySpaceRange doesn't exist, then we know that we only
                  -- got here because the range doesn't exist anywhere, including
                  -- NewKeySpaces. Thus, we just update the lat.
                  DS.slaveGroupRanges .^^ SGR.readAll timestamp
                  return ()
            PM.PickKeySpace slaveGroupId choice uid -> do
              DS.networkTaskManager . NTM.taskMap .^^. Mp.delete uid
              DS.slaveGroupRanges .^^ SGR.pick slaveGroupId choice
              return ()
        _ -> U.caseError
