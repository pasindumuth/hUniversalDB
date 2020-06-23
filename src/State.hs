{-# LANGUAGE RankNTypes #-}

module State where

import qualified Control.Monad.State as St
import qualified Records.Actions.Actions as A
import Control.Lens (Lens', (.~), (^.), (&))

infixl 1 .^
infixl 1 .^^
infixl 1 .^^.
infixl 1 .^^^

type ST s a = St.State ([A.OutputAction], s) a

runST :: ST s a -> s -> (a, ([A.OutputAction], s))
runST st s = St.runState st ([], s)

-- Adds actions. Importantly, this adds actions in reverse.
addA :: A.OutputAction -> ST s ()
addA a = St.state $ \(as, s) ->((), (a:as, s))

-- if we can find an identity lens, we can get rid of updateS and just
-- use .^^^.. We can also get rid of getId
updateS :: (s -> s) -> ST s s
updateS f = St.state $ \(as, state) -> (f state, (as, f state))

getId :: ST s s
getId = St.state $ \(as, state) -> (state, (as, state))

get :: Lens' s1 s2 -> ST s1 s2
get lens = St.state $ \(as, state) -> (state ^. lens, (as, state))

-- Dig, update, return, add Actions
(.^) :: Lens' s1 s2 -> ST s2 a -> ST s1 a
(.^) lens st = St.state $ \(as, state) ->
  let (ret, (as', subState)) = St.runState st (as, state ^. lens)
  in (ret, (as', state & lens .~ subState))

-- Dig, update, and return
(.^^) :: Lens' s1 s2 -> (s2 -> (a, s2)) -> ST s1 a
(.^^) lens func = St.state $ \(as, state) ->
  let (ret, subState) = state ^. lens & func
  in (ret, (as, state & lens .~ subState))

-- Dig and return
(.^^^) :: Lens' s1 s2 -> (s2 -> a) -> ST s1 a
(.^^^) lens func = St.state $ \(as, state) ->
  let ret = state ^. lens & func
  in (ret, (as, state))

-- Dig and update
(.^^.) :: Lens' s1 s2 -> (s2 -> s2) -> ST s1 s2
(.^^.) lens func = St.state $ \(as, state) ->
  let subState = state ^. lens & func
  in (subState, (as, state & lens .~ subState))

wrapMaybe :: ST s a -> ST (Maybe s) a
wrapMaybe st = St.state $ \(as, Just state) ->
  let (ret, (as', state')) = St.runState st (as, state)
  in (ret, (as', Just state'))
