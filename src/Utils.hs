module Utils where

import qualified Data.Sequence as Sq

for = flip map
s13 f a b c = f c a b


type Queue = Sq.Seq

push :: a -> Queue a -> Queue a
push val queue = queue Sq.|> val

poll :: Queue a -> (a, Queue a)
poll queue =
  let val Sq.:< queue' = Sq.viewl queue
  in (val, queue')
