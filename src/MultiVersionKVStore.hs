module MultiVersionKVStore (
  MS.MultiVersionKVStore,
  MultiVersionKVStore.read, -- qualified to avoid conflict with Prelude.read
  staticRead,
  staticReadLat,
  write,
) where

import qualified Control.Exception.Base as Ex

import qualified Records.MultiVersionKVStore as MS
import Lens

-- When a key is not present, that's not the fault of the caller. Subsequent reads
-- at that timestamp will always return a Nothing. 
read :: String -> Int -> MS.MultiVersionKVStore -> (Maybe String, MS.MultiVersionKVStore)
read key timestamp m =
  case m ^. at key of
    Just (versions, lat) ->
      case dropWhile (\v -> (v ^. _2) > timestamp) versions of
        ((value, _):_) -> (Just value, m & ix key . _2 %~ (max timestamp))
        [] -> (Nothing, m & ix key . _2 %~ (max timestamp))
    Nothing -> (Nothing, m & at key ?~ ([], timestamp))

-- This method is design to return the same thing every time for a 
-- given key and timestamp (hence why we throw fatal errors if the
-- key hasn't been seen or we are trying to read ahead of the `lat`).
staticRead :: String -> Int -> MS.MultiVersionKVStore -> Maybe String
staticRead key timestamp m =
  case m ^. at key of
    Just (versions, lat) ->
      Ex.assert (timestamp <= lat) $
      case dropWhile (\v -> (v ^. _2) > timestamp) versions of
        ((value, _):_) -> Just value
        _ -> Nothing
    _ -> Ex.assert False Nothing

-- This method isn't guarnteed to result in the same value every time for a given
-- key. In particular, a `Nothing` is retuend exactly when a key has never been seen before.
staticReadLat :: String -> MS.MultiVersionKVStore -> Maybe Int
staticReadLat key m =
  case m ^. at key of
    Just (_, lat) -> Just lat
    _ -> Nothing

write :: String -> String -> Int -> MS.MultiVersionKVStore -> ((), MS.MultiVersionKVStore)
write key value timestamp m =
  case m ^. at key of
    Nothing -> ((), m & at key ?~ ([(value, timestamp)], timestamp))
    Just (versions, lat) ->
      Ex.assert (timestamp > lat) $
      ((), m & ix key .~ ((value, timestamp):versions, timestamp))