{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Proto.Messages.ClientResponses.DeleteDatabase where

import qualified Data.Binary as Bn
import qualified GHC.Generics as Gn

import qualified Proto.Common as Co

data DeleteDatabase =
  BackwardsWriteMaster |
  BackwardsWriteSlave |
  DoesNotExist |
  NothingChanged |
  Success
  deriving (Gn.Generic, Bn.Binary, Show, Eq)
