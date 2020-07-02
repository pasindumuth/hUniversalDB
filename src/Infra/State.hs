{-# LANGUAGE RankNTypes #-}

module Infra.State (
  module Infra.State,
  module Infra.Internal_State
) where

import qualified Control.Monad.State as St
import qualified Debug.Trace as Tr
import Control.Lens (Lens', Traversal', (.~), (&), (^?!))

import qualified Proto.Actions.Actions as Ac
import Infra.Internal_State

type ST s a = STI ([Ac.OutputAction], Int) s a

runST :: ST s a -> s -> (a, ([Ac.OutputAction], s))
runST st s =
  let (ret, ((actions, _), state')) = St.runState st (([], 0), s)
  in (ret, (actions, state'))

debugP :: String -> ST s ()
debugP message =  St.state $ \((as, cnt), s) ->
  Tr.trace ("Trace L" ++ show cnt ++ ": " ++ message) ((), ((as, (cnt + 1)), s))

-- Adds actions. Importantly, this adds actions in reverse.
addA :: Ac.OutputAction -> ST s ()
addA a = St.state $ \((as, cnt), s) ->((), ((a:as, cnt), s))

-- Dig, update state, and return the return value and actions. 
runL :: Lens' s1 s2 -> ST s2 a -> ST s1 (a, [Ac.OutputAction])
runL lens st = St.state $ \((as, cnt), state) ->
  let (ret, ((as', cnt'), subState)) = St.runState st (([], cnt), state ^?! lens)
  in ((ret, as'), ((as, cnt'), state & lens .~ subState))

-- Dig, update state, and return the return value and actions from a Traversal
runT :: Traversal' s1 s2 -> ST s2 a -> ST s1 (a, [Ac.OutputAction])
runT traversal st = St.state $ \((as, cnt), state) ->
  let (ret, ((as', cnt'), subState)) = St.runState st (([], cnt), state ^?! traversal)
  in ((ret, as'), ((as, cnt'), state & traversal .~ subState))
