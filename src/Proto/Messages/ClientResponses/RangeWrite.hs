{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Proto.Messages.ClientResponses.RangeWrite where

import qualified Data.Binary as Bn
import qualified GHC.Generics as Gn

data RangeWrite =
  BackwardsWrite |
  Success
  deriving (Gn.Generic, Bn.Binary, Show)
