{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Proto.Messages.ClientResponses.CreateDatabase where

import qualified Data.Binary as Bn
import qualified GHC.Generics as Gn

import qualified Proto.Common as Co

data CreateDatabase =
  BackwardsWrite |
  AlreadyExists |
  NothingChanged |
  Success
  deriving (Gn.Generic, Bn.Binary, Show, Eq)
