{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Master.SlaveGroupRanges (
  SlaveGroupRanges,
  constructor,
  Value(..),
  Master.SlaveGroupRanges.read,
  staticReadLat,
  staticRead,
  readAll,
  staticReadAll,
  write,
  pick
) where

import qualified Control.Exception.Base as Ex
import qualified Data.Default as Df
import qualified Data.Map as Mp
import qualified GHC.Generics as Gn

import qualified Infra.Utils as U
import qualified Proto.Common as Co
import Infra.Lens

data Value =
  Changing Co.ChangingKeySpace |
  Old Co.KeySpace
  deriving (Show)

data SlaveGroupRanges = SlaveGroupRanges {
  _i'lat :: Co.Lat,
  _i'ranges :: Mp.Map Co.SlaveGroupId [(Co.Timestamp, Value)]
} deriving (Gn.Generic, Show)

makeLenses ''SlaveGroupRanges

-- TODO: eventually, this manual default definition should be made obselete
-- by an admin client API that allows for reconfiguration.
constructor
  :: [Co.SlaveGroupId]
  -> SlaveGroupRanges
constructor slaveGroupIds = SlaveGroupRanges {
  _i'lat = Df.def,
  _i'ranges = Mp.fromList $ map (\sGId -> (sGId, [])) slaveGroupIds
}

read
  :: Co.SlaveGroupId
  -> Co.Timestamp
  -> SlaveGroupRanges
  -> (Maybe (Co.Timestamp, Value), SlaveGroupRanges)
read slaveGroupId timestamp s =
  case Mp.lookup slaveGroupId (s ^. i'ranges) of
    Just versions ->
      let s' = s & i'lat .~ (max (s ^. i'lat) timestamp)
      in case dropWhile (\v -> (v ^. _1) > timestamp) versions of
        (version:_) -> (Just version, s')
        [] -> (Nothing, s')
    Nothing -> Ex.assert False (Nothing, s)

staticReadLat :: SlaveGroupRanges -> Co.Timestamp
staticReadLat s = s ^. i'lat

staticRead
  :: Co.SlaveGroupId
  -> Co.Timestamp
  -> SlaveGroupRanges
  -> Maybe (Co.Timestamp, Value)
staticRead slaveGroupId timestamp s =
  Ex.assert (timestamp <= s ^. i'lat) $
  case Mp.lookup slaveGroupId (s ^. i'ranges) of
    Just versions ->
      case dropWhile (\v -> (v ^. _1) > timestamp) versions of
        (version:_) -> Just version
        [] -> Nothing
    Nothing -> Ex.assert False Nothing

readAll
  :: Co.Timestamp
  -> SlaveGroupRanges
  -> (Mp.Map Co.SlaveGroupId (Maybe (Co.Timestamp, Value)), SlaveGroupRanges)
readAll timestamp s =
  (staticReadAll timestamp s, s & i'lat .~ (max (s ^. i'lat) timestamp))

-- TODO: call this unsafeReadAll, since it doesn't assert the timestamp
-- to be less than lat.
staticReadAll
  :: Co.Timestamp
  -> SlaveGroupRanges
  -> Mp.Map Co.SlaveGroupId (Maybe (Co.Timestamp, Value))
staticReadAll timestamp s =
  U.s12 Mp.map (s ^. i'ranges) $ \versions ->
    case dropWhile (\v -> (v ^. _1) > timestamp) versions of
      (version:_) -> Just version
      [] -> Nothing

write
  :: Co.Timestamp
  -> (Mp.Map Co.SlaveGroupId Co.KeySpace)
  -> SlaveGroupRanges
  -> ((), SlaveGroupRanges)
write timestamp newKeySpaces s =
  Ex.assert (timestamp > s ^. i'lat) $
  let s' =
        U.s31 Mp.foldlWithKey s newKeySpaces $ \s slaveGroupId keySpace ->
          case Mp.lookup slaveGroupId (s ^. i'ranges) of
            Just versions ->
              let changingKeySpace =
                    case versions of
                      ((timestamp', Changing _):_) -> Ex.assert False (Co.ChangingKeySpace [] [])
                      ((timestamp', Old oldKeySpace):_) -> Co.ChangingKeySpace oldKeySpace keySpace
                      [] -> Co.ChangingKeySpace [] keySpace
              in s & i'ranges . at slaveGroupId ?~ (timestamp, Changing changingKeySpace):versions
            Nothing -> Ex.assert False s
  in ((), s' & i'lat .~ timestamp)

pick
  :: Co.SlaveGroupId
  -> Co.Choice
  -> SlaveGroupRanges
  -> ((), SlaveGroupRanges)
pick slaveGroupId choice s =
  case Mp.lookup slaveGroupId (s ^. i'ranges) of
    Just ((timestamp', Changing (Co.ChangingKeySpace oldKeySpace newKeySpace)):rest) ->
      let keySpace =
            case choice of
              Co.NewChoice -> newKeySpace
              Co.OldChoice -> oldKeySpace
          s' = s & i'ranges . at slaveGroupId ?~ (timestamp', Old keySpace):rest
      in ((), s')
    _ -> Ex.assert False ((), s)
