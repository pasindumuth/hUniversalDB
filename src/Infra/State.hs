{-# LANGUAGE RankNTypes #-}

module Infra.State (
  module Infra.State,
  module Infra.Internal_State,
  ppShow
) where

import qualified Control.Monad.State as St
import qualified Debug.Trace as Tr
import Control.Lens (Lens', Traversal', (.~), (&), (^?!))
import Text.Show.Pretty (ppShow)

import qualified Proto.Actions.Actions as Ac
import qualified Proto.Actions.MasterActions as MAc
import qualified Proto.Actions.SlaveActions as SAc
import qualified Proto.Actions.TabletActions as TAc
import qualified Proto.Messages.TraceMessages as TrM
import Infra.Internal_State

-- Specific versions of ST
type STM s a = ST MAc.OutputAction s a -- For Master
type STS s a = ST SAc.OutputAction s a -- For Slave
type STT s a = ST TAc.OutputAction s a -- For Tablet

-- A General version of ST
type ST o s a = STI ([o], Int, [TrM.TraceMessage]) s a

-- This function reverses the actions and traceMsgs so that they are
-- presented to the caller in the FIFO order.
runST :: ST o s a -> s -> (a, ([o], [TrM.TraceMessage], s))
runST st s =
  let (ret, ((actions, _, traceMsgs), state')) = St.runState st (([], 0, []), s)
  in (ret, (reverse actions, reverse traceMsgs, state'))

debugP :: String -> ST o s ()
debugP message =  St.state $ \((as, cnt, trace), s) ->
  Tr.trace ("Trace L" ++ show cnt ++ ": " ++ message) ((), ((as, (cnt + 1), trace), s))

-- Adds actions. Importantly, this adds actions in reverse.
addA :: o -> ST o s ()
addA a = St.state $ \((as, cnt, trace), s) ->((), ((a:as, cnt, trace), s))

trace :: TrM.TraceMessage -> ST o s ()
trace traceMsg =  St.state $ \((as, cnt, trace), s) ->((), ((as, cnt, traceMsg:trace), s))

-- Dig, update state, and return the return value and actions.
-- This function also reverses that actions before returning them.
runL :: Lens' s1 s2 -> ST o s2 a -> ST o s1 (a, [o])
runL lens st = St.state $ \((as, cnt, trace), state) ->
  let (ret, ((as', cnt', trace'), subState)) = St.runState st (([], cnt, trace), state ^?! lens)
  in ((ret, reverse as'), ((as, cnt', trace'), state & lens .~ subState))

-- Dig, update state, and return the return value and actions from a Traversal.
-- This function also reverses that actions before returning them.
runT :: Traversal' s1 s2 -> ST o1 s2 a -> ST o2 s1 (a, [o1])
runT traversal st = St.state $ \((as, cnt, trace), state) ->
  let (ret, ((as', cnt', trace'), subState)) = St.runState st (([], cnt, trace), state ^?! traversal)
  in ((ret, reverse as'), ((as, cnt', trace'), state & traversal .~ subState))
