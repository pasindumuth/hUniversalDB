module Slave.KeySpaceManager (
  IKSM.KeySpaceManager,
  staticReadLat,
  staticRead
) where

import qualified Proto.Common as Co
import qualified Slave.Internal_KeySpaceManager as IKSM
import Infra.Lens

staticReadLat :: IKSM.KeySpaceManager -> Int
staticReadLat keySpaceManager = keySpaceManager ^. IKSM.lat

staticRead :: Int -> IKSM.KeySpaceManager -> Maybe (Co.Timestamp, Co.RequestId, [Co.KeySpaceRange])
staticRead timestamp keySpaceManager =
  case dropWhile (\v -> (v ^. _1) > timestamp) (keySpaceManager ^. IKSM.versions) of
    [] -> Nothing
    (version:_) -> Just version
