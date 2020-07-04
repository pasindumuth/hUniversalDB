{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Proto.Messages.TabletMessages where

import qualified Data.Binary as Bn
import qualified GHC.Generics as Gn

import qualified Proto.Common as Co
import qualified Proto.Messages.PaxosMessages as PM
import Infra.Lens

data RequestMeta = RequestMeta {
  _requestId :: Co.RequestId
} deriving (Gn.Generic, Bn.Binary, Show)

data RequestPayload =
  ReadRequest {
    key :: String,
    timestamp :: Int } |
  WriteRequest {
    key :: String,
    value :: String,
    timestamp :: Int }
  deriving (Gn.Generic, Bn.Binary, Show)

data ClientRequest = ClientRequest {
  _meta :: RequestMeta,
  _payload :: RequestPayload
} deriving (Gn.Generic, Bn.Binary, Show)

makeLenses ''RequestMeta
makeLenses ''ClientRequest

data TabletMessage =
  ForwardedClientRequest ClientRequest |
  MultiPaxosMessage PM.MultiPaxosMessage
  deriving (Gn.Generic, Bn.Binary, Show)
