{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Proto.Messages.ClientResponses.RangeRead where

import qualified Data.Binary as Bn
import qualified GHC.Generics as Gn

import qualified Proto.Common as Co

data RangeRead =
  Success [Co.KeySpaceRange]
  deriving (Gn.Generic, Bn.Binary, Show)
