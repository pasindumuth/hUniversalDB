{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Proto.Messages.TabletMessages where

import qualified Data.Binary as Bn
import qualified GHC.Generics as Gn

import qualified Proto.Messages.PaxosMessages as PM

data TabletMessage =
  MultiPaxosMessage PM.MultiPaxosMessage
  deriving (Gn.Generic, Bn.Binary, Show)
