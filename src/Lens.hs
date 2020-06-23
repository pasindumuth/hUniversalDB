{-# LANGUAGE RankNTypes #-}

module Lens(
  -- re-exorts from lens libriary
  makeLenses,
  (%~),
  (.~),
  (^.),
  (&),
  (?~),
  Lens',
  at,
  ix,
  lp2,
  lp3,
  _1, _2, _3,
  -- custom exports
  (.^.),
  wrapMaybe
) where

import Control.Lens (makeLenses, (%~), (.~), (^.), (&), (?~), Lens', _1, _2, _3, ALens', (^#), (<&>), (#~))
import Control.Lens.At (at, ix)

infixl 1 .^.

(.^.) :: s -> (Lens' s a) -> (a -> (b, a)) -> (b, s)
(.^.) state lens func =
  let (ret, subState) = state ^. lens & func
  in (ret, state & lens .~ subState)

wrapMaybe :: (a -> (c, b)) -> (Maybe a -> (c, Maybe b))
wrapMaybe f (Just x) =
  let (ret, state) = f x
  in (ret, Just state)

lp2 :: (ALens' s a1, ALens' s a2) -> Lens' s (a1, a2)
lp2 (l1, l2) f s =
    f (s ^# l1, s ^# l2) <&> \(a1, a2) -> s & l1 #~ a1 & l2 #~ a2

lp3 :: (ALens' s a1, ALens' s a2, ALens' s a3) -> Lens' s (a1, a2, a3)
lp3 (l1, l2, l3) f s =
    f (s ^# l1, s ^# l2, s ^# l3) <&> \(a1, a2, a3) -> s & l1 #~ a1 & l2 #~ a2 & l3 #~ a3
