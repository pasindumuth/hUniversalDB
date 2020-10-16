{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Common.MultiVersionMap (
  MultiVersionMap,
  Common.MultiVersionMap.read, -- qualified to avoid conflict with Prelude.read
  staticRead,
  staticReadLat,
  write,
) where

import qualified Control.Exception.Base as Ex
import qualified Data.Map as Mp

import qualified Proto.Common as Co
import Infra.Lens

type MultiVersionMap keyT valueT miscT = Mp.Map keyT (([(valueT, Co.Timestamp)], Co.Lat), Maybe miscT)

-- When a key is not present, that's not the fault of the caller. Subsequent reads
-- at that timestamp will always return a Nothing.
read
  :: (Ord keyT)
  => keyT
  -> Co.Timestamp
  -> MultiVersionMap keyT valueT miscT
  -> (Maybe (valueT, Co.Timestamp), MultiVersionMap keyT valueT miscT)
read key timestamp m =
  case m ^. at key of
    Just ((versions, lat), _) ->
      case dropWhile (\v -> (v ^. _2) > timestamp) versions of
        (version:_) -> (Just version, m & ix key . _1 . _2 %~ (max timestamp))
        [] -> (Nothing, m & ix key . _1 . _2 %~ (max timestamp))
    Nothing -> (Nothing, m & ix key . _1 .~ ([], timestamp))

-- This method is design to return the same thing every time for a
-- given key and timestamp (hence why we throw fatal errors if the
-- key hasn't been seen or we are trying to read ahead of the `lat`).
staticRead
  :: (Ord keyT)
  => keyT
  -> Co.Timestamp
  -> MultiVersionMap keyT valueT miscT
  -> Maybe (valueT, Co.Timestamp)
staticRead key timestamp m =
  case m ^. at key of
    Just ((versions, lat), _) ->
      Ex.assert (timestamp <= lat) $
      case dropWhile (\v -> (v ^. _2) > timestamp) versions of
        (version:_) -> Just version
        _ -> Nothing
    _ -> Ex.assert False Nothing

-- This method returns Nothing if the key doesn't exist. Otherwise,
-- it returns the lat.
staticReadLat
  :: (Ord keyT)
  => keyT
  -> MultiVersionMap keyT valueT miscT
  -> Maybe Int
staticReadLat key m =
  case m ^. at key of
    Just ((_, lat), _) -> Just lat
    _ -> Nothing

write
  :: (Ord keyT)
  => keyT
  -> valueT
  -> Co.Timestamp
  -> MultiVersionMap keyT valueT miscT
  -> ((), MultiVersionMap keyT valueT miscT)
write key value timestamp m =
  case m ^. at key of
    Nothing -> ((), m & at key ?~ (([(value, timestamp)], timestamp), Nothing))
    Just ((versions, lat), _) ->
      Ex.assert (timestamp > lat) $
      ((), m & ix key .~ (((value, timestamp):versions, timestamp), Nothing))
