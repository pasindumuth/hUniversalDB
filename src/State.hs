{-# LANGUAGE RankNTypes #-}

module State where

import qualified Control.Monad.State as St
import qualified Records.Actions.Actions as Ac
import Control.Lens (Lens', Traversal', (.~), (&), (^?!))

infixl 1 .^
infixl 1 .^^
infixl 1 .^^.
infixl 1 .^^^

infixl 1 .^*
infixl 1 .^^*
infixl 1 .^^.*
infixl 1 .^^^*

type ST s a = St.State ([Ac.OutputAction], s) a

runST :: ST s a -> s -> (a, ([Ac.OutputAction], s))
runST st s = St.runState st ([], s)

makeST :: a -> ST s a
makeST ret = St.state $ \(as, s) -> (ret, (as, s))

-- Adds actions. Importantly, this adds actions in reverse.
addA :: Ac.OutputAction -> ST s ()
addA a = St.state $ \(as, s) ->((), (a:as, s))

getL :: Lens' s1 s2 -> ST s1 s2
getL lens = St.state $ \(as, state) -> (state ^?! lens, (as, state))

getT :: Traversal' s1 s2 -> ST s1 s2
getT traversal = St.state $ \(as, state) -> (state ^?! traversal, (as, state))

-- Dig, update state, and return the return value and actions. 
runL :: Lens' s1 s2 -> ST s2 a -> ST s1 (a, [Ac.OutputAction])
runL lens st = St.state $ \(as, state) ->
  let (ret, (as', subState)) = St.runState st ([], state ^?! lens)
  in ((ret, as'), (as, state & lens .~ subState))

-- Dig, update state, and return the return value and actions from a Traversal
runT :: Traversal' s1 s2 -> ST s2 a -> ST s1 (a, [Ac.OutputAction])
runT traversal st = St.state $ \(as, state) ->
  let (ret, (as', subState)) = St.runState st ([], state ^?! traversal)
  in ((ret, as'), (as, state & traversal .~ subState))

-- Dig, update, return, add Actions
(.^) :: Lens' s1 s2 -> ST s2 a -> ST s1 a
(.^) lens st = St.state $ \(as, state) ->
  let (ret, (as', subState)) = St.runState st (as, state ^?! lens)
  in (ret, (as', state & lens .~ subState))

-- Dig, update, return, add Actions from a Traversal.
(.^*) :: Traversal' s1 s2 -> ST s2 a -> ST s1 a
(.^*) traversal st = St.state $ \(as, state) ->
  let (ret, (as', subState)) = St.runState st (as, state ^?! traversal)
  in (ret, (as', state & traversal .~ subState))

-- Dig, update, and return
(.^^) :: Lens' s1 s2 -> (s2 -> (a, s2)) -> ST s1 a
(.^^) lens func = St.state $ \(as, state) ->
  let (ret, subState) = state ^?! lens & func
  in (ret, (as, state & lens .~ subState))

-- Dig, update, and return from a Traversal.
(.^^*) :: Traversal' s1 s2 -> (s2 -> (a, s2)) -> ST s1 a
(.^^*) traversal func = St.state $ \(as, state) ->
  let (ret, subState) = state ^?! traversal & func
  in (ret, (as, state & traversal .~ subState))

-- Dig and return
(.^^^) :: Lens' s1 s2 -> (s2 -> a) -> ST s1 a
(.^^^) lens func = St.state $ \(as, state) ->
  let ret = state ^?! lens & func
  in (ret, (as, state))

-- Dig and return from a Traversal.
(.^^^*) :: Traversal' s1 s2 -> (s2 -> a) -> ST s1 a
(.^^^*) traversal func = St.state $ \(as, state) ->
  let ret = state ^?! traversal & func
  in (ret, (as, state))

-- Dig and update
(.^^.) :: Lens' s1 s2 -> (s2 -> s2) -> ST s1 s2
(.^^.) lens func = St.state $ \(as, state) ->
  let subState = state ^?! lens & func
  in (subState, (as, state & lens .~ subState))

-- Dig and update from a Traversal.
(.^^.*) :: Traversal' s1 s2 -> (s2 -> s2) -> ST s1 s2
(.^^.*) traversal func = St.state $ \(as, state) ->
  let subState = state ^?! traversal & func
  in (subState, (as, state & traversal .~ subState))
