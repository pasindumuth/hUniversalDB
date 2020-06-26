{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Records.Messages.Messages where

import qualified Data.Binary as Bn
import qualified Data.Default as Df
import qualified GHC.Generics as Gn

import qualified Records.Messages.ClientMessages as CM
import qualified Records.Messages.PaxosMessages as PM

-- TODO maybe we can use prisms to construct Message from one of the constituents
data Message =
  ClientRequest CM.ClientRequest |
  ClientResponse CM.ClientResponse |
  MultiPaxosMessage PM.MultiPaxosMessage
  deriving (Gn.Generic, Bn.Binary, Show)
