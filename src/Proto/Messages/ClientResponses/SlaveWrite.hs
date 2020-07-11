{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Proto.Messages.ClientResponses.SlaveWrite where

import qualified Data.Binary as Bn
import qualified GHC.Generics as Gn

data SlaveWrite =
  UnknownDB |
  BackwardsWrite |
  Success
  deriving (Gn.Generic, Bn.Binary, Show, Eq)
