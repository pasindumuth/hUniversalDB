{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Proto.Messages.TraceMessages where

import qualified Data.Binary as Bn
import qualified GHC.Generics as Gn

import qualified Proto.Common as Co
import qualified Proto.Messages.PaxosMessages as PM
import qualified Proto.Messages.ClientMessages as CM

data TraceMessage =
  PaxosInsertion {
    paxosId :: Co.PaxosId,
    index :: PM.IndexT,
    entry :: PM.PaxosLogEntry } |
  ClientRequestReceived CM.ClientRequest |
  ClientResponseSent CM.ClientResponse
  deriving (Gn.Generic, Bn.Binary, Show)
