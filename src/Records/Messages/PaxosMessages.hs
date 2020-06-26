{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Records.Messages.PaxosMessages where

import qualified Data.Binary as Bn
import qualified Data.Default as Df
import qualified GHC.Generics as Gn

data PaxosLogEntry =
  Read { key :: String, timestamp :: Int } |
  Write { key :: String, value :: String, timestamp :: Int }
  deriving (Gn.Generic, Bn.Binary, Show, Eq)

 -- TODO get rid of this by updating Acceptor to have a `Maybe val`
instance Df.Default PaxosLogEntry where
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
  deriving (Gn.Generic, Bn.Binary, Show)

data MultiPaxosMessage =
  Insert { val :: PaxosLogEntry } |
  PaxosMessage { index :: IndexT, message :: PaxosMessage }
  deriving (Gn.Generic, Bn.Binary, Show)
