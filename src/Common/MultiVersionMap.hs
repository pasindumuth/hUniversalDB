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

type MultiVersionMap keyT valueT = Mp.Map keyT ([(Maybe valueT, Co.Timestamp)], Co.Lat)

-- When a key is not present, that's not the fault of the caller. Subsequent reads
-- at that timestamp will always return a Nothing.
read
  :: (Ord keyT)
  => keyT
  -> Co.Timestamp
  -> MultiVersionMap keyT valueT
  -> (Maybe (valueT, Co.Timestamp), MultiVersionMap keyT valueT)
read key timestamp m =
  case m ^. at key of
    Just (versions, lat) ->
      case dropWhile (\v -> (v ^. _2) > timestamp) versions of
        ((Just value, timestamp):_) -> (Just (value, timestamp), m & ix key . _2 %~ (max timestamp))
        _ -> (Nothing, m & ix key . _2 %~ (max timestamp))
    Nothing -> (Nothing, m & ix key .~ ([], timestamp))

-- This method is design to return the same thing every time for a
-- given key and timestamp (hence why we throw fatal errors if the
-- key hasn't been seen or we are trying to read ahead of the `lat`).
staticRead
  :: (Ord keyT)
  => keyT
  -> Co.Timestamp
  -> MultiVersionMap keyT valueT
  -> Maybe (valueT, Co.Timestamp)
staticRead key timestamp m =
  case m ^. at key of
    Just (versions, lat) ->
      Ex.assert (timestamp <= lat) $
      case dropWhile (\v -> (v ^. _2) > timestamp) versions of
        ((Just value, timestamp):_) -> Just (value, timestamp)
        _ -> Nothing
    _ -> Ex.assert False Nothing

-- This method returns Nothing if the key doesn't exist. Otherwise,
-- it returns the lat.
staticReadLat
  :: (Ord keyT)
  => keyT
  -> MultiVersionMap keyT valueT
  -> Maybe Int
staticReadLat key m =
  case m ^. at key of
    Just (_, lat) -> Just lat
    _ -> Nothing

write
  :: (Ord keyT)
  => keyT
  -> Maybe valueT
  -> Co.Timestamp
  -> MultiVersionMap keyT valueT
  -> ((), MultiVersionMap keyT valueT)
write key value timestamp m =
  case m ^. at key of
    Nothing -> ((), m & at key ?~ ([(value, timestamp)], timestamp))
    Just (versions, lat) ->
      Ex.assert (timestamp > lat) $
      ((), m & ix key .~ ((value, timestamp):versions, timestamp))
