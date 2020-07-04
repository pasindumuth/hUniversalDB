{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Proto.Messages.ClientMessages where

import qualified Data.Binary as Bn
import qualified Data.Default as Df
import qualified GHC.Generics as Gn

import qualified Proto.Common as Co
import Infra.Lens

data RequestMeta = RequestMeta {
  requestId :: Co.RequestId
} deriving (Gn.Generic, Bn.Binary, Show)

data RequestPayload =
  CreateDatabase {
    databaseId :: Co.DatabaseId,
    tableId :: Co.TableId } |
  ReadRequest {
    databaseId :: Co.DatabaseId,
    tableId :: Co.TableId,
    key :: String,
    timestamp :: Int } |
  WriteRequest {
    databaseId :: Co.DatabaseId,
    tableId :: Co.TableId,
    key :: String,
    value :: String,
    timestamp :: Int }
  deriving (Gn.Generic, Bn.Binary, Show)

data ClientRequest = ClientRequest {
  requestMeta :: RequestMeta,
  requestPayload :: RequestPayload
} deriving (Gn.Generic, Bn.Binary, Show)

data ResponseMeta = ResponseMeta {
  requestId :: Co.RequestId
} deriving (Gn.Generic, Bn.Binary, Show)

data ResponsePayload =
  Error { message :: String } |
  ReadResponse { value :: Maybe String } |
  WriteResponse |
  Success
  deriving (Gn.Generic, Bn.Binary, Show)

data ClientResponse = ClientResponse {
  responseMeta :: ResponseMeta,
  responsePayload :: ResponsePayload
} deriving (Gn.Generic, Bn.Binary, Show)
