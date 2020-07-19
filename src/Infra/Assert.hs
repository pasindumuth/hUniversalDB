module Infra.Assert where

-- I decided to use this assert instead of the standard
-- Control.Exception.assert because sometimes, it wouldn't
-- get triggered (probably due to laziness). Although this doesn't
-- autotmatically get turned off during compiler optimization, we can
-- simply replace the body of this function with Control.Exception.assert
-- when we want to make an optimized build.
assert :: Bool -> a -> a
assert bool val =
  assertMsg bool "Assertion Failed" val

assertMsg :: Bool -> String -> a -> a
assertMsg bool msg val =
  if bool
    then val
    else error msg
