module Infra.Utils where

import qualified Data.Binary as Bn
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import qualified Control.Exception as Ex
import qualified Control.Monad as Mo
import qualified Data.Sequence as Sq
import qualified Data.Map as Mp
import qualified Data.Set as St
import qualified System.Random as Rn

import qualified Proto.Common as Co
import qualified Infra.Assert as As

------------------------------------------------------------------------------------------------------------------------
-- Queue
------------------------------------------------------------------------------------------------------------------------
-- | This is a simple Queuing data structure that we crated
-- by simply using a Seq, where we trivially wrap
-- the Seq operations.

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

------------------------------------------------------------------------------------------------------------------------
-- Error Handling
------------------------------------------------------------------------------------------------------------------------

-- | To see the location of the case error, build the program using the
-- --profile flag. The INLINE pragma helps improve the callstack.
caseError :: a
caseError = error "Case Error: reached an unhandled case in the case statement."
{-# INLINE caseError #-}

------------------------------------------------------------------------------------------------------------------------
-- Random
------------------------------------------------------------------------------------------------------------------------
randomS :: St.Set a -> Rn.StdGen -> (a, Rn.StdGen)
randomS set rg =
  As.assert (St.size set > 0) $
  let (r, rg') = Rn.randomR (0, (St.size set) - 1) rg
  in (St.elemAt r set, rg')

randomL :: [a] -> Rn.StdGen -> (a, Rn.StdGen)
randomL list rg =
  As.assert (length list > 0) $
  let (r, rg') = Rn.randomR (0, (length list) - 1) rg
  in (list !! r, rg')

randomM :: Mp.Map k a -> Rn.StdGen -> ((k, a), Rn.StdGen)
randomM mp rg =
  As.assert (Mp.size mp > 0) $
  let (r, rg') = Rn.randomR (0, (Mp.size mp) - 1) rg
  in (Mp.elemAt r mp, rg')

------------------------------------------------------------------------------------------------------------------------
-- Argument Swapping
------------------------------------------------------------------------------------------------------------------------
-- | We often want to swap the arguments when calling a function.
-- Here, we define some basic common swappings for 2 and 3 argument
-- functions.

s12 f a b = f b a
s31 f a b c = f c a b
s13 f a b c = f b c a

------------------------------------------------------------------------------------------------------------------------
-- Argument-Reversed Map/Fold
------------------------------------------------------------------------------------------------------------------------
-- | These are simply analogous version of foldM, fold, and map, except
-- where the function-argument is put the end. This way, we can pass
-- in a multi-line lambda.

foldM :: (Foldable t, Monad m) => b -> t a -> (b -> a -> m b) -> m b
foldM = s31 Mo.foldM

fold :: Foldable t => b -> t a -> (b -> a -> b) -> b
fold = s31 Prelude.foldl

map :: [a] -> (a -> b) -> [b]
map = s12 Prelude.map

------------------------------------------------------------------------------------------------------------------------
-- ByteString Encoding/Decoding
------------------------------------------------------------------------------------------------------------------------
encode :: Bn.Binary a => a -> BS.ByteString
encode b = BSL.toStrict $ Bn.encode b

decode :: Bn.Binary a => BS.ByteString -> a
decode d = Bn.decode $ BSL.fromStrict d

------------------------------------------------------------------------------------------------------------------------
-- Loop
------------------------------------------------------------------------------------------------------------------------
-- | The definition of the Loop Monad is identical to Either a b,
-- where Break corresponds to Left, and Continue corresponds to Right.
-- We prefer Loop over Either because it's more readable when we use foldM
-- to implement a for-loop with early termination.

data Loop a b = Break a | Continue b
  deriving (Eq, Ord, Read, Show)

instance Functor (Loop a) where
    fmap _ (Break x) = Break x
    fmap f (Continue y) = Continue (f y)

instance Semigroup (Loop a b) where
    Break _ <> b = b
    a <> _ = a

instance Applicative (Loop e) where
    pure = Continue
    Break e <*> _ = Break e
    Continue f <*> r = fmap f r

instance Monad (Loop e) where
    Break l >>= _ = Break l
    Continue r >>= k = k r

------------------------------------------------------------------------------------------------------------------------
-- Miscellaneous
------------------------------------------------------------------------------------------------------------------------
prefix :: (Eq a) => a -> [a] -> [a]
prefix s [] = []
prefix s (x:xs) =
  if s == x
    then []
    else x : prefix s xs

mkUID :: Rn.StdGen -> (Co.UID, Rn.StdGen)
mkUID rg =
  let (uid, rg') =
        s31 foldl ([], rg) [1..8] $ \(uid, rg) _ ->
          let (r, rg') = Rn.random rg
          in (r:uid, rg')
  in (Co.UID uid, rg')
