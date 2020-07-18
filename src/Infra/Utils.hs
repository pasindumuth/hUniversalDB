module Infra.Utils where

import qualified Control.Exception as Ex
import qualified Data.Sequence as Sq
import qualified Data.Set as St
import qualified System.Random as Rn

import qualified Proto.Common as Co

for = flip map
s12 f a b = f b a
s31 f a b c = f c a b
s13 f a b c = f b c a

type Queue = Sq.Seq

push :: a -> Queue a -> Queue a
push val queue = queue Sq.|> val

poll :: Queue a -> (a, Queue a)
poll queue =
  let val Sq.:< queue' = Sq.viewl queue
  in (val, queue')

peek :: Queue a -> a
peek queue =
  let val Sq.:< _ = Sq.viewl queue
  in val

prefix :: (Eq a) => a -> [a] -> [a]
prefix s [] = []
prefix s (x:xs) =
  if s == x
    then []
    else x : prefix s xs

randomS :: St.Set a -> Rn.StdGen -> (a, Rn.StdGen)
randomS set rg =
  Ex.assert (St.size set > 0) $
  let (r, rg') = Rn.randomR (0, (St.size set) - 1) rg
  in (St.elemAt r set, rg')

randomL :: [a] -> Rn.StdGen -> (a, Rn.StdGen)
randomL list rg =
  Ex.assert (length list > 0) $
  let (r, rg') = Rn.randomR (0, (length list) - 1) rg
  in (list !! r, rg')

-- To see the location of the case error, build the program using the
-- --profile flag. The INLINE pragma helps improve the callstack.
caseError :: a
caseError = error "Case Error: reached an unhandled case in the case statement."
{-# INLINE caseError #-}

mkUID :: Rn.StdGen -> (Co.UID, Rn.StdGen)
mkUID rg =
  s31 foldl ([], rg) [1..8] $ \(uid, rg) _ ->
    let (r, rg') = Rn.random rg
    in (r:uid, rg')
