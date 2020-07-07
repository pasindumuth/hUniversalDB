{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Proto.Messages.ClientResponses where

import qualified Data.Binary as Bn
import qualified Data.Default as Df
import qualified GHC.Generics as Gn

import qualified Proto.Common as Co
import Infra.Lens

data Meta = Meta {
  _requestId :: Co.RequestId
} deriving (Gn.Generic, Bn.Binary, Show)

data Payload =
  Error { message :: String } |
  Read { value :: Maybe String } |
  Write |
  Success
  deriving (Gn.Generic, Bn.Binary, Show)

data ClientResponse = ClientResponse {
  _responseMeta :: Meta,
  _responsePayload :: Payload
} deriving (Gn.Generic, Bn.Binary, Show)

makeLenses ''Meta
makeLenses '' ClientResponse
