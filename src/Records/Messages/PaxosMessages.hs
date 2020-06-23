{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Records.Messages.PaxosMessages where

import qualified Data.Binary as B
import qualified Data.Default as D
import qualified GHC.Generics as G

data PaxosLogEntry =
  Read { key :: String, timestamp :: Int } |
  Write { key :: String, value :: String, timestamp :: Int }
  deriving (G.Generic, B.Binary, Show, Eq)

type IndexT = Int
type Rnd = Int
type Val = PaxosLogEntry

data PaxosMessage =
  Propose { crnd :: Rnd, cval :: Val } |
  Prepare { crnd :: Rnd} |
  Promise { crnd :: Rnd, vrnd :: Rnd, vval :: Val } |
  Accept { crnd :: Rnd, cval :: Val } |
  Learn { lrnd :: Rnd, lval :: Val }
  deriving (G.Generic, B.Binary, Show)

data MultiPaxosMessage =
  Insert { val :: PaxosLogEntry } |
  PaxosMessage { index :: IndexT, message :: PaxosMessage }
  deriving (G.Generic, B.Binary, Show)
