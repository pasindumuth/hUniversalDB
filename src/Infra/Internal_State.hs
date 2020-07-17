{-# LANGUAGE RankNTypes #-}

module Infra.Internal_State where

import qualified Control.Monad.State as St

import Control.Lens (Lens', Traversal', (.~), (&), (^?!), has)

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

-- TODO: Optimization: we should eliminate this check for an optimized build.
-- Ex.assert can't replace this; it just gets skipped when is use ix with a non-existent
-- key. Unfortunately, we can't use the technique in caseError to make the location
-- of where the error occured more visible. We'll just have to do a full-text
-- search and use print statements.
existsAssert :: Bool -> a -> a
existsAssert bool val =
  if bool
    then val
    else error "Exists error: the element does not exist in the Traversal structure."

-- Dig, update, return, update auxiliary data
(.^) :: Lens' s1 s2 -> STI aux s2 a -> STI aux s1 a
(.^) lens st = St.state $ \(aux, state) ->
  let (ret, (aux', subState)) = St.runState st (aux, state ^?! lens)
  in (ret, (aux', state & lens .~ subState))

-- Dig, update, return, update auxiliary data from a Traversal.
(.^*) :: Traversal' s1 s2 -> STI aux s2 a -> STI aux s1 a
(.^*) traversal st = St.state $ \(aux, state) ->
  existsAssert (has traversal state) $
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
  existsAssert (has traversal state) $
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
  existsAssert (has traversal state) $
  let ret = state ^?! traversal & func
  in (ret, (aux, state))

-- Dig and update
(.^^.) :: Lens' s1 s2 -> (s2 -> s2) -> STI aux s1 s2
(.^^.) lens func = St.state $ \(aux, state) ->
  let subState = state ^?! lens & func
  in (subState, (aux, state & lens .~ subState))

-- Dig and update from a Traversal.
(.^^.*) :: Traversal' s1 s2 -> (s2 -> s2) -> STI aux s1 s2
(.^^.*) traversal func = St.state $ \(aux, state) ->
  existsAssert (has traversal state) $
  let subState = state ^?! traversal & func
  in (subState, (aux, state & traversal .~ subState))
