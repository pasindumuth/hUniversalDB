{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Slave.KeySpaceManager (
  KeySpaceManager,
  Slave.KeySpaceManager.read,
  write,
  staticReadLat,
  staticRead,
  allRanges
) where

import qualified Control.Exception.Base as Ex
import qualified Data.Default as Df
import qualified Data.Set as St
import qualified GHC.Generics as Gn

import qualified Infra.Utils as U
import qualified Proto.Common as Co
import Infra.Lens

data KeySpaceManager = KeySpaceManager {
  _i'lat :: Int,
  _i'versions :: [(Co.Timestamp, Co.RequestId, [(Co.KeySpaceRange, Co.TabletId)])],
  _i'allRanges :: St.Set Co.TabletId
} deriving (Gn.Generic, Df.Default, Show)

makeLenses ''KeySpaceManager

-- Add a read and write operation here, and use asserts. Make it safe.
-- Then, use this during testing. We don't need to test this itself, I guess.
read
  :: Int
  -> KeySpaceManager
  -> (Maybe (Co.Timestamp, Co.RequestId, [(Co.KeySpaceRange, Co.TabletId)]), KeySpaceManager)
read timestamp keySpaceManager =
  let version = case dropWhile (\v -> (v ^. _1) > timestamp) (keySpaceManager ^. i'versions) of
                  [] -> Nothing
                  (version:_) -> Just version
  in (version, keySpaceManager & i'lat %~ (max timestamp))

staticReadLat :: KeySpaceManager -> Int
staticReadLat keySpaceManager = keySpaceManager ^. i'lat

staticRead
  :: Co.Timestamp
  -> KeySpaceManager
  -> Maybe (Co.Timestamp, Co.RequestId, [(Co.KeySpaceRange, Co.TabletId)])
staticRead timestamp keySpaceManager =
  Ex.assert (timestamp <= (keySpaceManager ^. i'lat)) $
  case dropWhile (\v -> (v ^. _1) > timestamp) (keySpaceManager ^. i'versions) of
    [] -> Nothing
    (version:_) -> Just version

write
  :: Co.Timestamp
  -> Co.RequestId
  -> [(Co.KeySpaceRange, Co.TabletId)]
  -> KeySpaceManager
  -> ((), KeySpaceManager)
write timestamp requestId ranges keySpaceManager =
  Ex.assert (timestamp > (keySpaceManager ^. i'lat)) $
  ((), keySpaceManager & i'lat .~ timestamp
                       & i'versions %~ ((timestamp, requestId, ranges):)
                       & (U.s13 foldl ranges $ \keySpaceManager (_, tabletId) ->
                           keySpaceManager & i'allRanges %~ (St.insert tabletId)))

allRanges :: KeySpaceManager -> St.Set Co.TabletId
allRanges = (^. i'allRanges)
