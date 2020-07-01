{-# LANGUAGE RankNTypes #-}

module Infra.State where

import qualified Control.Monad.State as St
import qualified Debug.Trace as Tr

import qualified Proto.Actions.Actions as Ac
import Control.Lens (Lens', Traversal', (.~), (&), (^?!))

infixl 1 .^
infixl 1 .^^
infixl 1 .^^.
infixl 1 .^^^

infixl 1 .^*
infixl 1 .^^*
infixl 1 .^^.*
infixl 1 .^^^*

type ST s a = St.State ([Ac.OutputAction], Int, s) a

runST :: ST s a -> s -> (a, ([Ac.OutputAction], s))
runST st s =
  let (ret, (actions, _, state')) = St.runState st ([], 0, s)
  in (ret, (actions, state'))

makeST :: a -> ST s a
makeST ret = St.state $ \(as, cnt, s) -> (ret, (as, cnt, s))

trace :: String -> ST s ()
trace message =  St.state $ \(as, cnt, s) ->
  Tr.trace ("Trace L" ++ show cnt ++ ": " ++ message) ((), (as, (cnt + 1), s))

-- Adds actions. Importantly, this adds actions in reverse.
addA :: Ac.OutputAction -> ST s ()
addA a = St.state $ \(as, cnt, s) ->((), (a:as, cnt, s))

getL :: Lens' s1 s2 -> ST s1 s2
getL lens = St.state $ \(as, cnt, state) -> (state ^?! lens, (as, cnt, state))

getT :: Traversal' s1 s2 -> ST s1 s2
getT traversal = St.state $ \(as, cnt, state) -> (state ^?! traversal, (as, cnt, state))

-- Dig, update state, and return the return value and actions. 
runL :: Lens' s1 s2 -> ST s2 a -> ST s1 (a, [Ac.OutputAction])
runL lens st = St.state $ \(as, cnt, state) ->
  let (ret, (as', cnt', subState)) = St.runState st ([], cnt, state ^?! lens)
  in ((ret, as'), (as, cnt', state & lens .~ subState))

-- Dig, update state, and return the return value and actions from a Traversal
runT :: Traversal' s1 s2 -> ST s2 a -> ST s1 (a, [Ac.OutputAction])
runT traversal st = St.state $ \(as, cnt, state) ->
  let (ret, (as', cnt', subState)) = St.runState st ([], cnt, state ^?! traversal)
  in ((ret, as'), (as, cnt', state & traversal .~ subState))

-- Dig, update, return, add Actions
(.^) :: Lens' s1 s2 -> ST s2 a -> ST s1 a
(.^) lens st = St.state $ \(as, cnt, state) ->
  let (ret, (as', cnt', subState)) = St.runState st (as, cnt, state ^?! lens)
  in (ret, (as', cnt', state & lens .~ subState))

-- Dig, update, return, add Actions from a Traversal.
(.^*) :: Traversal' s1 s2 -> ST s2 a -> ST s1 a
(.^*) traversal st = St.state $ \(as, cnt, state) ->
  let (ret, (as', cnt', subState)) = St.runState st (as, cnt, state ^?! traversal)
  in (ret, (as', cnt', state & traversal .~ subState))

-- Dig, update, and return
(.^^) :: Lens' s1 s2 -> (s2 -> (a, s2)) -> ST s1 a
(.^^) lens func = St.state $ \(as, cnt, state) ->
  let (ret, subState) = state ^?! lens & func
  in (ret, (as, cnt, state & lens .~ subState))

-- Dig, update, and return from a Traversal.
(.^^*) :: Traversal' s1 s2 -> (s2 -> (a, s2)) -> ST s1 a
(.^^*) traversal func = St.state $ \(as, cnt, state) ->
  let (ret, subState) = state ^?! traversal & func
  in (ret, (as, cnt, state & traversal .~ subState))

-- Dig and return
(.^^^) :: Lens' s1 s2 -> (s2 -> a) -> ST s1 a
(.^^^) lens func = St.state $ \(as, cnt, state) ->
  let ret = state ^?! lens & func
  in (ret, (as, cnt, state))

-- Dig and return from a Traversal.
(.^^^*) :: Traversal' s1 s2 -> (s2 -> a) -> ST s1 a
(.^^^*) traversal func = St.state $ \(as, cnt, state) ->
  let ret = state ^?! traversal & func
  in (ret, (as, cnt, state))

-- Dig and update
(.^^.) :: Lens' s1 s2 -> (s2 -> s2) -> ST s1 s2
(.^^.) lens func = St.state $ \(as, cnt, state) ->
  let subState = state ^?! lens & func
  in (subState, (as, cnt, state & lens .~ subState))

-- Dig and update from a Traversal.
(.^^.*) :: Traversal' s1 s2 -> (s2 -> s2) -> ST s1 s2
(.^^.*) traversal func = St.state $ \(as, cnt, state) ->
  let subState = state ^?! traversal & func
  in (subState, (as, cnt, state & traversal .~ subState))
