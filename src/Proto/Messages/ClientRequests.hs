{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Proto.Messages.ClientRequests where

import qualified Data.Binary as Bn
import qualified Data.Default as Df
import qualified GHC.Generics as Gn

import qualified Proto.Common as Co
import Infra.Lens

data RequestMeta = RequestMeta {
  _requestId :: Co.RequestId
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
  _meta :: RequestMeta,
  _payload :: RequestPayload
} deriving (Gn.Generic, Bn.Binary, Show)

makeLenses ''RequestMeta
makeLenses '' ClientRequest
