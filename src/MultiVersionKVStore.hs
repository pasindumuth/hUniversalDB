module MultiVersionKVStore where

import qualified Control.Exception.Base as Ex
import qualified Data.Map as Mp
import qualified Message as M

import Lens ((%~), (.~), (^.), (&), (?~), at, ix, _2)

type Version = (M.Value, M.Timestamp)
type Versions = [Version] -- versions are stored in reverse order
type Lat = M.Timestamp -- Last Access Time

type MultiVersionKVStore = Mp.Map M.Key (Versions, Lat)

read :: String -> Int -> MultiVersionKVStore -> (Maybe String, MultiVersionKVStore)
read key timestamp m =
  case m ^. at key of
    Nothing -> (Nothing, m & at key ?~ ([], timestamp))
    Just (versions, lat) ->
      case dropWhile (\v -> (v ^. _2) > timestamp) versions of
        [] -> (Nothing, m & ix key . _2 %~ (max timestamp))
        ((value, _):_) -> (Just value, m & ix key . _2 %~ (max timestamp))

readLat :: String -> MultiVersionKVStore -> Maybe Int
readLat key m =
  case m ^. at key of
    Nothing -> Nothing
    Just (_, lat) -> Just lat

write :: String -> String -> Int -> MultiVersionKVStore -> ((), MultiVersionKVStore)
write key value timestamp m =
  case m ^. at key of
    Nothing -> ((), m & at key ?~ ([(value, timestamp)], timestamp))
    Just (versions, lat) ->
      Ex.assert (timestamp > lat) $
      ((), m & ix key .~ ((value, timestamp):versions, timestamp))