{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Message where

import qualified Data.Binary as B
import qualified Data.Default as D
import qualified GHC.Generics as G

data PaxosLogEntry = Read | Write String
  deriving (G.Generic, B.Binary, Show)

 -- TODO get rid of this by updating Acceptor to have a `Maybe val`
instance D.Default PaxosLogEntry where
  def = Read

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