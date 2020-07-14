{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Master.SlaveGroupRanges (
  SlaveGroupRanges,
  Value(..),
  Master.SlaveGroupRanges.read,
  staticReadLat,
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

data Choice = NewChoice | OldChoice

data Value =
  New Co.NewKeySpace |
  Old Co.KeySpace
  deriving (Show)

data SlaveGroupRanges = SlaveGroupRanges {
  _i'lat :: Co.Lat,
  _i'ranges :: Mp.Map Co.SlaveGroupId [(Co.Timestamp, Value)]
} deriving (Gn.Generic, Df.Default, Show)

makeLenses ''SlaveGroupRanges

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
              let newKeySpace =
                    case versions of
                      ((timestamp', New _):_) -> Ex.assert False (Co.NewKeySpace [] [])
                      ((timestamp', Old oldKeySpace):_) -> Co.NewKeySpace oldKeySpace keySpace
                      [] -> Co.NewKeySpace [] keySpace
              in s & i'ranges . at slaveGroupId ?~ (timestamp, New newKeySpace):versions
            Nothing -> Ex.assert False s
  in ((), s' & i'lat .~ timestamp)

pick
  :: Co.SlaveGroupId
  -> Choice
  -> SlaveGroupRanges
  -> ((), SlaveGroupRanges)
pick slaveGroupId choice s =
  case Mp.lookup slaveGroupId (s ^. i'ranges) of
    Just ((timestamp', New (Co.NewKeySpace newKeySpace oldKeySpace)):rest) ->
      let keySpace =
            case choice of
              NewChoice -> newKeySpace
              OldChoice -> oldKeySpace
          s' = s & i'ranges . at slaveGroupId ?~ (timestamp', Old keySpace):rest
      in ((), s')
    _ -> Ex.assert False ((), s)
