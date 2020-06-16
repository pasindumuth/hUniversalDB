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
  lensProduct,
  _1, _2,
  -- custom exports
  (.^.),
  wrapMaybe
) where

import Control.Lens (makeLenses, (%~), (.~), (^.), (&), (?~), Lens', _1, _2)
import Control.Lens.At (at, ix)
import Control.Lens.Unsound (lensProduct)

infixl 1 .^.

(.^.) :: s -> (Lens' s a) -> (a -> (b, a)) -> (b, s)
(.^.) state lens func =
  let (ret, subState) = state ^. lens & func
  in (ret, state & lens .~ subState)

wrapMaybe :: (a -> (c, b)) -> (Maybe a -> (c, Maybe b))
wrapMaybe f (Just x) =
  let (ret, state) = f x
  in (ret, Just state)
