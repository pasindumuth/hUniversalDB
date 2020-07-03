{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Proto.Messages.ClientMessages where

import qualified Data.Binary as Bn
import qualified Data.Default as Df
import qualified GHC.Generics as Gn

import qualified Proto.Common as Co

data ClientRequest =
  CreateDatabase {
    requestId :: Co.RequestId,
    databaseId :: Co.DatabaseId,
    tableId :: Co.TableId } |
  ReadRequest {
    requestId :: Co.RequestId,
    databaseId :: Co.DatabaseId,
    tableId :: Co.TableId,
    key :: String,
    timestamp :: Int } |
  WriteRequest {
    requestId :: Co.RequestId,
    databaseId :: Co.DatabaseId,
    tableId :: Co.TableId,
    key :: String,
    value :: String,
    timestamp :: Int }
  deriving (Gn.Generic, Bn.Binary, Show)

data ClientResponse =
  Error {
    requestId :: Co.RequestId,
    message :: String } |
  ReadResponse {
    requestId :: Co.RequestId,
    value :: Maybe String } |
  WriteResponse {
    requestId :: Co.RequestId } |
  Success {
    requestId :: Co.RequestId }
  deriving (Gn.Generic, Bn.Binary, Show)
