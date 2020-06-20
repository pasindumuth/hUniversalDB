{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Message where

import qualified Data.Binary as B
import qualified Data.Default as D
import qualified GHC.Generics as G

type Key = String
type Value = String
type Timestamp = Int

data PaxosLogEntry = Read Key Timestamp | Write Key Value Timestamp
  deriving (G.Generic, B.Binary, Show, Eq)

 -- TODO get rid of this by updating Acceptor to have a `Maybe val`
instance D.Default PaxosLogEntry where
  def = Read "" 0

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
  PMessage { index :: IndexT, message :: PaxosMessage }
  deriving (G.Generic, B.Binary, Show)

data Message =
  MMessage MultiPaxosMessage |
  ClientMessage String
  deriving (G.Generic, B.Binary, Show)

data ClientRequest =
  CRead Key Timestamp |
  CWrite Key Value Timestamp
  deriving (G.Generic, B.Binary, Show, Eq)

data Retry = Retry {
  try :: Int
} deriving (Show)
