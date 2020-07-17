{-# LANGUAGE RankNTypes #-}

module Infra.Internal_State where

import qualified Control.Monad.State as St

import Control.Lens (Lens', Traversal', (.~), (&), (^?!))

infixl 1 .^
infixl 1 .^^
infixl 1 .^^.
infixl 1 .^^^

infixl 1 .^*
infixl 1 .^^*
infixl 1 .^^.*
infixl 1 .^^^*

type STI aux s a = St.State (aux, s) a

makeST :: a -> STI aux s a
makeST ret = St.state $ \(aux, s) -> (ret, (aux, s))

getL :: Lens' s1 s2 -> STI aux s1 s2
getL lens = St.state $ \(aux, state) -> (state ^?! lens, (aux, state))

getT :: Traversal' s1 s2 -> STI aux s1 s2
getT traversal = St.state $ \(aux, state) -> (state ^?! traversal, (aux, state))

-- Dig, update, return, update auxiliary data
(.^) :: Lens' s1 s2 -> STI aux s2 a -> STI aux s1 a
(.^) lens st = St.state $ \(aux, state) ->
  let (ret, (aux', subState)) = St.runState st (aux, state ^?! lens)
  in (ret, (aux', state & lens .~ subState))

-- Dig, update, return, update auxiliary data from a Traversal.
(.^*) :: Traversal' s1 s2 -> STI aux s2 a -> STI aux s1 a
(.^*) traversal st = St.state $ \(aux, state) ->
  let (ret, (aux', subState)) = St.runState st (aux, state ^?! traversal)
  in (ret, (aux', state & traversal .~ subState))

-- Dig, update, and return
(.^^) :: Lens' s1 s2 -> (s2 -> (a, s2)) -> STI aux s1 a
(.^^) lens func = St.state $ \(aux, state) ->
  let (ret, subState) = state ^?! lens & func
  in (ret, (aux, state & lens .~ subState))

-- Dig, update, and return from a Traversal.
(.^^*) :: Traversal' s1 s2 -> (s2 -> (a, s2)) -> STI aux s1 a
(.^^*) traversal func = St.state $ \(aux, state) ->
  let (ret, subState) = state ^?! traversal & func
  in (ret, (aux, state & traversal .~ subState))

-- Dig and return
(.^^^) :: Lens' s1 s2 -> (s2 -> a) -> STI aux s1 a
(.^^^) lens func = St.state $ \(aux, state) ->
  let ret = state ^?! lens & func
  in (ret, (aux, state))

-- Dig and return from a Traversal.
(.^^^*) :: Traversal' s1 s2 -> (s2 -> a) -> STI aux s1 a
(.^^^*) traversal func = St.state $ \(aux, state) ->
  let ret = state ^?! traversal & func
  in (ret, (aux, state))

-- Dig and update
(.^^.) :: Lens' s1 s2 -> (s2 -> s2) -> STI aux s1 s2
(.^^.) lens func = St.state $ \(aux, state) ->
  let subState = state ^?! lens & func
  in (subState, (aux, state & lens .~ subState))

-- Dig and update from a Traversal.
-- TODO: if we use ix to access a value that's not there, but the value
-- is a type of Monoid, we won't get an error here; it will automatically
-- create one. Maybe add some error checking here.
(.^^.*) :: Traversal' s1 s2 -> (s2 -> s2) -> STI aux s1 s2
(.^^.*) traversal func = St.state $ \(aux, state) ->
  let subState = state ^?! traversal & func
  in (subState, (aux, state & traversal .~ subState))
