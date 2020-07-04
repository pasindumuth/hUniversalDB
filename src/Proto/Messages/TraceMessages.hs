{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Proto.Messages.TraceMessages where

import qualified Data.Binary as Bn
import qualified GHC.Generics as Gn

import qualified Proto.Common as Co
import qualified Proto.Messages.PaxosMessages as PM
import qualified Proto.Messages.ClientRequests as CRq
import qualified Proto.Messages.ClientResponses as CRs

data TraceMessage =
  PaxosInsertion {
    paxosId :: Co.PaxosId,
    index :: PM.IndexT,
    entry :: PM.PaxosLogEntry } |
  ClientRequestReceived CRq.ClientRequest |
  ClientResponseSent CRs.ClientResponse
  deriving (Gn.Generic, Bn.Binary, Show)
