module PaxosLog (
  P.PaxosLog,
  insert,
  nextAvailableIndex,
  newlyAddedEntries,
  plog,
) where

import qualified Data.Map as Mp
import qualified Data.Maybe as Mb
import qualified Control.Exception.Base as Ex
import qualified Data.Set as S

import qualified Message as M
import qualified Utils as U
import qualified Records.PaxosLog as P
import Lens ((^.), (&), at)

insert :: M.IndexT -> M.PaxosLogEntry -> P.PaxosLog -> P.PaxosLog
insert index val p =
  Ex.assert (Mb.isNothing $ p ^. P.plog & Mp.lookup index) $
  Ex.assert ((p ^. P.availableIndices & S.size) > 0) $
  let lastAvailableIndex = p ^. P.availableIndices & S.elemAt ((p ^. P.availableIndices & S.size) - 1)
      plog' = p ^. P.plog & Mp.insert index val
      availableIndices' =
        if index < lastAvailableIndex
          then p ^. P.availableIndices & S.delete index
          else if index == lastAvailableIndex
            then p ^. P.availableIndices & S.delete index & S.insert (index + 1)
            else p ^. P.availableIndices & S.union $ S.fromList $ [lastAvailableIndex + 1 .. index - 1] ++ [index + 1]
  in P.PaxosLog plog' availableIndices'

nextAvailableIndex :: P.PaxosLog -> M.IndexT
nextAvailableIndex p =
  Ex.assert ((p ^. P.availableIndices & S.size) > 0) $
  p ^. P.availableIndices & S.elemAt 0

newlyAddedEntries :: P.PaxosLog -> P.PaxosLog -> [(M.IndexT, M.PaxosLogEntry)]
newlyAddedEntries pl pl' =
  let index = pl & nextAvailableIndex
      index' = pl' & nextAvailableIndex
  in U.for [index .. index' - 1] $ \i -> (i, pl' ^. P.plog . at i & Mb.fromJust)

plog :: P.PaxosLog -> Mp.Map M.IndexT M.PaxosLogEntry
plog pl = pl ^. P.plog
