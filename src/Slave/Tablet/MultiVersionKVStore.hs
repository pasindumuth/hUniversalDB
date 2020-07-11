module Slave.Tablet.MultiVersionKVStore (
  MS.MultiVersionKVStore,
  Slave.Tablet.MultiVersionKVStore.read, -- qualified to avoid conflict with Prelude.read
  staticRead,
  staticReadLat,
  write,
) where

import qualified Control.Exception.Base as Ex

import qualified Proto.Common as Co
import qualified Slave.Tablet.Internal_MultiVersionKVStore as MS
import Infra.Lens

-- When a key is not present, that's not the fault of the caller. Subsequent reads
-- at that timestamp will always return a Nothing. 
read :: String -> Int -> MS.MultiVersionKVStore -> (Maybe (Co.Value, Co.RequestId), MS.MultiVersionKVStore)
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
staticRead :: String -> Int -> MS.MultiVersionKVStore -> Maybe (Co.Value, Co.RequestId, Co.Timestamp)
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
staticReadLat :: String -> MS.MultiVersionKVStore -> Maybe Int
staticReadLat key m =
  case m ^. at key of
    Just (_, lat) -> Just lat
    _ -> Nothing

write :: Co.Key -> Co.Value -> Co.RequestId -> Co.Timestamp -> MS.MultiVersionKVStore -> ((), MS.MultiVersionKVStore)
write key value requestId timestamp m =
  case m ^. at key of
    Nothing -> ((), m & at key ?~ ([(value, requestId, timestamp)], timestamp))
    Just (versions, lat) ->
      Ex.assert (timestamp > lat) $
      ((), m & ix key .~ ((value, requestId, timestamp):versions, timestamp))