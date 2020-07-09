module Slave.KeySpaceManager where

import qualified Proto.Common as Co
import qualified Slave.Internal_KeySpaceManager as IKSM
import Infra.Lens

staticReadLat :: IKSM.KeySpaceManager -> Int
staticReadLat keySpaceManager = keySpaceManager ^. IKSM.lat

staticRead :: Int -> IKSM.KeySpaceManager -> [Co.KeySpaceRange]
staticRead timestamp keySpaceManager =
  case dropWhile (\v -> (v ^. _1) > timestamp) (keySpaceManager ^. IKSM.versions) of
    [] -> []
    ((_, _, ranges):_) -> ranges
