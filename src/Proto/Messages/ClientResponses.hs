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

data ReadResponse =
  ReadSuccess { value :: Maybe String } |
  ReadUnknownDB
  deriving (Gn.Generic, Bn.Binary, Show)

data WriteResponse =
  WriteSuccess |
  WriteUnknownDB |
  BackwardsWrite
  deriving (Gn.Generic, Bn.Binary, Show)

data CreateDBResponse =
  CreateDBSuccess
  deriving (Gn.Generic, Bn.Binary, Show)

data Payload =
  ReadResponse ReadResponse |
  WriteResponse WriteResponse |
  CreateDBResponse CreateDBResponse
  deriving (Gn.Generic, Bn.Binary, Show)

data ClientResponse = ClientResponse {
  _responseMeta :: Meta,
  _responsePayload :: Payload
} deriving (Gn.Generic, Bn.Binary, Show)

makeLenses ''Meta
makeLenses '' ClientResponse
