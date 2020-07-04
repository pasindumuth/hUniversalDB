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

data ResponseMeta = ResponseMeta {
  _requestId :: Co.RequestId
} deriving (Gn.Generic, Bn.Binary, Show)

data ResponsePayload =
  Error { message :: String } |
  ReadResponse { value :: Maybe String } |
  WriteResponse |
  Success
  deriving (Gn.Generic, Bn.Binary, Show)

data ClientResponse = ClientResponse {
  _responseMeta :: ResponseMeta,
  _responsePayload :: ResponsePayload
} deriving (Gn.Generic, Bn.Binary, Show)

makeLenses ''ResponseMeta
makeLenses '' ClientResponse
