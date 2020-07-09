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

data Meta = Meta {
  _requestId :: Co.RequestId
} deriving (Gn.Generic, Bn.Binary, Show)

data Payload =
  TabletRead {
    key :: String,
    timestamp :: Int } |
  TabletWrite {
    key :: String,
    value :: String,
    timestamp :: Int }
  deriving (Gn.Generic, Bn.Binary, Show)

data ClientRequest = ClientRequest {
  _meta :: Meta,
  _payload :: Payload
} deriving (Gn.Generic, Bn.Binary, Show)

makeLenses ''Meta
makeLenses ''ClientRequest

data TabletMessage =
  ForwardedClientRequest ClientRequest |
  MultiPaxosMessage PM.MultiPaxosMessage
  deriving (Gn.Generic, Bn.Binary, Show)
