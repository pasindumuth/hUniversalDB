module MultiVersionKVStore (
  MS.MultiVersionKVStore,
  MultiVersionKVStore.read, -- qualified to avoid conflict with Prelude.read
  readLat,
  write,
) where

import qualified Control.Exception.Base as Ex

import qualified Records.MultiVersionKVStore as MS
import Lens ((%~), (.~), (^.), (&), (?~), at, ix, _2)

read :: String -> Int -> MS.MultiVersionKVStore -> (Maybe String, MS.MultiVersionKVStore)
read key timestamp m =
  case m ^. at key of
    Nothing -> (Nothing, m & at key ?~ ([], timestamp))
    Just (versions, lat) ->
      case dropWhile (\v -> (v ^. _2) > timestamp) versions of
        [] -> (Nothing, m & ix key . _2 %~ (max timestamp))
        ((value, _):_) -> (Just value, m & ix key . _2 %~ (max timestamp))

readLat :: String -> MS.MultiVersionKVStore -> Maybe Int
readLat key m =
  case m ^. at key of
    Nothing -> Nothing
    Just (_, lat) -> Just lat

write :: String -> String -> Int -> MS.MultiVersionKVStore -> ((), MS.MultiVersionKVStore)
write key value timestamp m =
  case m ^. at key of
    Nothing -> ((), m & at key ?~ ([(value, timestamp)], timestamp))
    Just (versions, lat) ->
      Ex.assert (timestamp > lat) $
      ((), m & ix key .~ ((value, timestamp):versions, timestamp))