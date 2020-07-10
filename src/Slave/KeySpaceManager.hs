module Slave.KeySpaceManager (
  IKSM.KeySpaceManager,
  Slave.KeySpaceManager.read,
  write,
  staticReadLat,
  staticRead,
  allRanges
) where

import qualified Control.Exception.Base as Ex
import qualified Data.Set as St

import qualified Infra.Utils as U
import qualified Proto.Common as Co
import qualified Slave.Internal_KeySpaceManager as IKSM
import Infra.Lens

-- Add a read and write operation here, and use asserts. Make it safe.
-- Then, use this during testing. We don't need to test this itself, I guess.
read :: Int -> IKSM.KeySpaceManager -> (Maybe (Co.Timestamp, Co.RequestId, [Co.KeySpaceRange]), IKSM.KeySpaceManager)
read timestamp keySpaceManager =
  let version = case dropWhile (\v -> (v ^. _1) > timestamp) (keySpaceManager ^. IKSM.versions) of
                  [] -> Nothing
                  (version:_) -> Just version
  in (version, keySpaceManager & IKSM.lat %~ (max timestamp))

staticReadLat :: IKSM.KeySpaceManager -> Int
staticReadLat keySpaceManager = keySpaceManager ^. IKSM.lat

staticRead :: Int -> IKSM.KeySpaceManager -> Maybe (Co.Timestamp, Co.RequestId, [Co.KeySpaceRange])
staticRead timestamp keySpaceManager =
  Ex.assert (timestamp <= (keySpaceManager ^. IKSM.lat)) $
  case dropWhile (\v -> (v ^. _1) > timestamp) (keySpaceManager ^. IKSM.versions) of
    [] -> Nothing
    (version:_) -> Just version

write :: Co.Timestamp -> Co.RequestId -> [Co.KeySpaceRange] -> IKSM.KeySpaceManager -> ((), IKSM.KeySpaceManager)
write timestamp requestId ranges keySpaceManager =
  Ex.assert (timestamp > (keySpaceManager ^. IKSM.lat)) $
  ((), keySpaceManager & IKSM.lat .~ timestamp
                       & IKSM.versions %~ ((timestamp, requestId, ranges):)
                       & (U.s13 foldl ranges $ \keySpaceManager range ->
                           keySpaceManager & IKSM.allRanges %~ (St.insert range)))

allRanges :: IKSM.KeySpaceManager -> St.Set Co.KeySpaceRange
allRanges = (^. IKSM.allRanges)
