module Slave.Tablet.MultiVersionKVStore (
  MultiVersionKVStore,
  Slave.Tablet.MultiVersionKVStore.read, -- qualified to avoid conflict with Prelude.read
  staticRead,
  staticReadLat,
  write,
) where

import qualified Control.Exception.Base as Ex
import qualified Data.Map as Mp

import qualified Proto.Common as Co
import Infra.Lens

type MultiVersionKVStore = Mp.Map Co.Key ([(Co.Value, Co.RequestId, Co.Timestamp)], Co.Lat)

-- When a key is not present, that's not the fault of the caller. Subsequent reads
-- at that timestamp will always return a Nothing. 
read :: String -> Int -> MultiVersionKVStore -> (Maybe (Co.Value, Co.RequestId), MultiVersionKVStore)
read key timestamp m =
  case m ^. at key of
    Just (versions, lat) ->
      case dropWhile (\v -> (v ^. _3) > timestamp) versions of
        ((value, requestId, _):_) -> (Just (value, requestId), m & ix key . _2 %~ (max timestamp))
        [] -> (Nothing, m & ix key . _2 %~ (max timestamp))
    Nothing -> (Nothing, m & at key ?~ ([], timestamp))

-- This method is design to return the same thing every time for a 
-- given key and timestamp (hence why we throw fatal errors if the
-- key hasn't been seen or we are trying to read ahead of the `lat`).
staticRead :: String -> Int -> MultiVersionKVStore -> Maybe (Co.Value, Co.RequestId, Co.Timestamp)
staticRead key timestamp m =
  case m ^. at key of
    Just (versions, lat) ->
      Ex.assert (timestamp <= lat) $
      case dropWhile (\v -> (v ^. _3) > timestamp) versions of
        ((value, requestId, timestamp'):_) -> Just (value, requestId, timestamp')
        _ -> Nothing
    _ -> Ex.assert False Nothing

-- This method isn't guarnteed to result in the same value every time for a given
-- key. In particular, a `Nothing` is retuend exactly when a key has never been seen before.
staticReadLat :: String -> MultiVersionKVStore -> Maybe Int
staticReadLat key m =
  case m ^. at key of
    Just (_, lat) -> Just lat
    _ -> Nothing

write :: Co.Key -> Co.Value -> Co.RequestId -> Co.Timestamp -> MultiVersionKVStore -> ((), MultiVersionKVStore)
write key value requestId timestamp m =
  case m ^. at key of
    Nothing -> ((), m & at key ?~ ([(value, requestId, timestamp)], timestamp))
    Just (versions, lat) ->
      Ex.assert (timestamp > lat) $
      ((), m & ix key .~ ((value, requestId, timestamp):versions, timestamp))