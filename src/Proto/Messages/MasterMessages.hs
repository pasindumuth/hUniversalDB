{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Proto.Messages.MasterMessages where

import qualified Data.Binary as Bn
import qualified GHC.Generics as Gn

import qualified Proto.Messages.PaxosMessages as PM

data MasterMessage =
  MultiPaxosMessage PM.MultiPaxosMessage
  deriving (Gn.Generic, Bn.Binary, Show)
