{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Proto.Messages.PaxosMessages where

import qualified Data.Binary as Bn
import qualified Data.Default as Df
import qualified GHC.Generics as Gn

import qualified Proto.Common as Co

data PaxosLogEntry =
  -- Tablet
  Tablet_Read {
    key :: String,
    timestamp :: Int } |
  Tablet_Write {
    key :: String,
    value :: String,
    timestamp :: Int } |
  -- Slave
  Slave_KeySpaceChange {
    oldRanges :: [Co.KeySpaceRange],
    newRanges :: [Co.KeySpaceRange],
    generation :: Int }
  deriving (Gn.Generic, Bn.Binary, Show, Eq)

type IndexT = Int
type Rnd = Int
type Val = PaxosLogEntry

data PaxosMessage =
  Propose { crnd :: Rnd, cval :: Val } |
  Prepare { crnd :: Rnd} |
  Promise { crnd :: Rnd, vrnd :: Rnd, vval :: Maybe Val } |
  Accept { crnd :: Rnd, cval :: Val } |
  Learn { lrnd :: Rnd, lval :: Val }
  deriving (Gn.Generic, Bn.Binary, Show)

data MultiPaxosMessage =
  Insert { val :: PaxosLogEntry } |
  PaxosMessage { index :: IndexT, message :: PaxosMessage }
  deriving (Gn.Generic, Bn.Binary, Show)
