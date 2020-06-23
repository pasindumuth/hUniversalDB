{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Records.Messages.Messages where

import qualified Data.Binary as B
import qualified Data.Default as D
import qualified GHC.Generics as G

import qualified Records.Common.Common as C
import qualified Records.Messages.ClientMessages as CM
import qualified Records.Messages.PaxosMessages as PM

data Message =
  ClientRequest CM.ClientRequest |
  ClientResponse CM.ClientResponse |
  MultiPaxosMessage PM.MultiPaxosMessage
  deriving (G.Generic, B.Binary, Show)
