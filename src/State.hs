{-# LANGUAGE RankNTypes #-}

module State where

import qualified Control.Monad.State as St
import qualified Records.Actions.Actions as A
import Control.Lens (Lens', Traversal', (.~), (^.), (&))

infixl 1 .^
infixl 1 .^^
infixl 1 .^^.
infixl 1 .^^^

infixl 1 .^*
infixl 1 .^^*
infixl 1 .^^.*
infixl 1 .^^^*

type ST s a = St.State ([A.OutputAction], s) a

runST :: ST s a -> s -> (a, ([A.OutputAction], s))
runST st s = St.runState st ([], s)

makeST :: a -> ST s a
makeST ret = St.state $ \(as, s) -> (ret, (as, s))

-- Adds actions. Importantly, this adds actions in reverse.
addA :: A.OutputAction -> ST s ()
addA a = St.state $ \(as, s) ->((), (a:as, s))

get :: Lens' s1 s2 -> ST s1 s2
get lens = St.state $ \(as, state) -> (state ^. lens, (as, state))

-- Dig, update state, and return the return value and actions. 
getA :: Lens' s1 s2 -> ST s2 a -> ST s1 (a, [A.OutputAction])
getA lens st = St.state $ \(as, state) ->
  let (ret, (as', subState)) = St.runState st ([], state ^. lens)
  in ((ret, as'), (as, state & lens .~ subState))

-- Dig, update, return, add Actions
(.^) :: Lens' s1 s2 -> ST s2 a -> ST s1 a
(.^) lens st = St.state $ \(as, state) ->
  let (ret, (as', subState)) = St.runState st (as, state ^. lens)
  in (ret, (as', state & lens .~ subState))

-- Dig, update, return, add Actions from a Traversal.
(.^*) :: (Monoid s2) => Traversal' s1 s2 -> ST s2 a -> ST s1 a
(.^*) lens st = St.state $ \(as, state) ->
  let (ret, (as', subState)) = St.runState st (as, state ^. lens)
  in (ret, (as', state & lens .~ subState))

-- Dig, update, and return
(.^^) :: Lens' s1 s2 -> (s2 -> (a, s2)) -> ST s1 a
(.^^) lens func = St.state $ \(as, state) ->
  let (ret, subState) = state ^. lens & func
  in (ret, (as, state & lens .~ subState))

-- Dig, update, and return from a Traversal.
(.^^*) :: (Monoid s2) => Traversal' s1 s2 -> (s2 -> (a, s2)) -> ST s1 a
(.^^*) lens func = St.state $ \(as, state) ->
  let (ret, subState) = state ^. lens & func
  in (ret, (as, state & lens .~ subState))

-- Dig and return
(.^^^) :: Lens' s1 s2 -> (s2 -> a) -> ST s1 a
(.^^^) lens func = St.state $ \(as, state) ->
  let ret = state ^. lens & func
  in (ret, (as, state))

-- Dig and return from a Traversal.
(.^^^*) :: (Monoid s2) => Traversal' s1 s2 -> (s2 -> a) -> ST s1 a
(.^^^*) lens func = St.state $ \(as, state) ->
  let ret = state ^. lens & func
  in (ret, (as, state))

-- Dig and update
(.^^.) :: Lens' s1 s2 -> (s2 -> s2) -> ST s1 s2
(.^^.) lens func = St.state $ \(as, state) ->
  let subState = state ^. lens & func
  in (subState, (as, state & lens .~ subState))

-- Dig and update from a Traversal.
(.^^.*) :: (Monoid s2) => Traversal' s1 s2 -> (s2 -> s2) -> ST s1 s2
(.^^.*) lens func = St.state $ \(as, state) ->
  let subState = state ^. lens & func
  in (subState, (as, state & lens .~ subState))

wrapMaybe :: ST s a -> ST (Maybe s) a
wrapMaybe st = St.state $ \(as, Just state) ->
  let (ret, (as', state')) = St.runState st (as, state)
  in (ret, (as', Just state'))
