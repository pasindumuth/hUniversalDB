{-# LANGUAGE RankNTypes #-}

module Infra.StateWithTrace (
  module Infra.StateWithTrace,
  module Infra.StateCore
) where

import qualified Control.Monad.State as St
import qualified Debug.Trace as Tr
import Control.Lens (Lens', Traversal', (.~), (&), (^?!), has)

import qualified Proto.Messages.TraceMessages as TrM
import Infra.StateCore

-- A General version of ST
--
-- `o` is OutputActions, which almost essentially a way for our code
-- to do side effects (like network requests). The evaluator of the ST will
-- enumerate through these OutputActions and execute them.
--
-- `s` is the State that is passed through the ST monad. This is the state that
-- we would our Do Expressions to be able to have access (read) and manipulate (write)
--
-- `a` is the return value of the ST execution. Usually, there is a final value
-- that is returned. If there isn't, it will be ().
type ST o t s a = STI ([o], Int, [t]) s a

-- This function reverses the actions and traceMsgs so that they are
-- presented to the caller in the FIFO order.
runST :: ST o t s a -> s -> (a, ([o], [t], s))
runST st s =
  let (ret, ((actions, _, traceMsgs), state')) = St.runState st (([], 0, []), s)
  in (ret, (reverse actions, reverse traceMsgs, state'))

debugP :: String -> a -> ST o t s a
debugP message output =  St.state $ \((as, cnt, trace), s) ->
  (Tr.trace ("Trace L" ++ show cnt ++ ": " ++ message) output, ((as, (cnt + 1), trace), s))

-- Adds actions. Importantly, this adds actions in reverse.
addA :: o -> ST o t s ()
addA a = St.state $ \((as, cnt, trace), s) ->((), ((a:as, cnt, trace), s))

trace :: t -> ST o t s ()
trace traceMsg =  St.state $ \((as, cnt, trace), s) ->((), ((as, cnt, traceMsg:trace), s))

-- Dig, update state, and return the return value and actions.
-- This function also reverses that actions before returning them.
runL :: Lens' s1 s2 -> ST o1 t s2 a -> ST o2 t s1 (a, [o1])
runL lens st = St.state $ \((as, cnt, trace), state) ->
  let (ret, ((as', cnt', trace'), subState)) = St.runState st (([], cnt, trace), state ^?! lens)
  in ((ret, reverse as'), ((as, cnt', trace'), state & lens .~ subState))

-- Dig, update state, and return the return value and actions from a Traversal.
-- This function also reverses that actions before returning them.
runT :: Traversal' s1 s2 -> ST o1 t s2 a -> ST o2 t s1 (a, [o1])
runT traversal st = St.state $ \((as, cnt, trace), state) ->
  existsAssert (has traversal state) $
  let (ret, ((as', cnt', trace'), subState)) = St.runState st (([], cnt, trace), state ^?! traversal)
  in ((ret, reverse as'), ((as, cnt', trace'), state & traversal .~ subState))
