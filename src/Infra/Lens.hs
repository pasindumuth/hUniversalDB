{-# LANGUAGE RankNTypes #-}

module Infra.Lens (
  makeLenses,
  (%~), (.~), (^.), (&), (?~), (^?!),
  (%^^), (%^^*), (%^^^), (%^^^*),
  Lens', Getter,
  at, ix,
  lp0, lp2, lp3, lp4, lp5, lp6,
  _1, _2, _3, _4, _5, _6,
) where

import Control.Lens (
  makeLenses,
  (%~), (.~), (^.), (&), (?~), (^?!),
  Lens', Traversal', ALens', ASetter, Getter,
  _1, _2, _3, _4, _5, _6,
  (^#), (<&>), (#~))
import Control.Lens.At (at, ix)

infixr 4 %^^
infixr 4 %^^*
infixr 4 %^^^
infixr 4 %^^^*

lp0 :: Lens' s ()
lp0 f s =
    f () <&> \() -> s

lp2 :: (ALens' s a1, ALens' s a2) -> Lens' s (a1, a2)
lp2 (l1, l2) f s =
    f (s ^# l1, s ^# l2) <&> \(a1, a2) -> s & l1 #~ a1 & l2 #~ a2

lp3 :: (ALens' s a1, ALens' s a2, ALens' s a3) -> Lens' s (a1, a2, a3)
lp3 (l1, l2, l3) f s =
    f (s ^# l1, s ^# l2, s ^# l3) <&> \(a1, a2, a3) -> s & l1 #~ a1 & l2 #~ a2 & l3 #~ a3

lp4 :: (ALens' s a1, ALens' s a2, ALens' s a3, ALens' s a4) -> Lens' s (a1, a2, a3, a4)
lp4 (l1, l2, l3, l4) f s =
    f (s ^# l1, s ^# l2, s ^# l3, s ^# l4) <&> \(a1, a2, a3, a4) -> s & l1 #~ a1 & l2 #~ a2 & l3 #~ a3 & l4 #~ a4

lp5 :: (ALens' s a1, ALens' s a2, ALens' s a3, ALens' s a4, ALens' s a5) -> Lens' s (a1, a2, a3, a4, a5)
lp5 (l1, l2, l3, l4, l5) f s =
    f (s ^# l1, s ^# l2, s ^# l3, s ^# l4, s ^# l5) <&> \(a1, a2, a3, a4, a5) -> s & l1 #~ a1 & l2 #~ a2 & l3 #~ a3 & l4 #~ a4 & l5 #~ a5

lp6 :: (ALens' s a1, ALens' s a2, ALens' s a3, ALens' s a4, ALens' s a5, ALens' s a6) -> Lens' s (a1, a2, a3, a4, a5, a6)
lp6 (l1, l2, l3, l4, l5, l6) f s =
    f (s ^# l1, s ^# l2, s ^# l3, s ^# l4, s ^# l5, s ^# l6) <&> \(a1, a2, a3, a4, a5, a6) -> s & l1 #~ a1 & l2 #~ a2 & l3 #~ a3 & l4 #~ a4 & l5 #~ a5 & l6 #~ a6

(%^^) :: s -> Lens' s a -> (a -> (r, a)) -> (r, s)
(%^^) state lens f =
  let (ret, state') = f (state ^?! lens)
  in (ret, state & lens .~ state')

(%^^*) :: s -> Traversal' s a -> (a -> (r, a)) -> (r, s)
(%^^*) state traversal f =
  let (ret, state') = f (state ^?! traversal)
  in (ret, state & traversal .~ state')

(%^^^) :: s -> Lens' s a -> (a -> r) -> r
(%^^^) state lens func = state ^?! lens & func

(%^^^*) :: s -> Traversal' s a -> (a -> r) -> r
(%^^^*) state traversal func = state ^?! traversal & func
