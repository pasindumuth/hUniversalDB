{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Proto.Messages.ClientMessages where

import qualified Data.Binary as Bn
import qualified Data.Default as Df
import qualified GHC.Generics as Gn

data ClientRequest =
  CreateDatabase {
    requestId :: String,
    databaseId :: String,
    tableId :: String } |
  ReadRequest {
    requestId :: String,
    databaseId :: String,
    tableId :: String,
    key :: String,
    timestamp :: Int } |
  WriteRequest {
    requestId :: String,
    databaseId :: String,
    tableId :: String,
    key :: String,
    value :: String,
    timestamp :: Int }
  deriving (Gn.Generic, Bn.Binary, Show)

data ClientResponse =
  Error {
    requestId :: String,
    message :: String } |
  ReadResponse {
    requestId :: String,
    value :: Maybe String } |
  WriteResponse {
    requestId :: String } |
  Success {
    requestId :: String }
  deriving (Gn.Generic, Bn.Binary, Show)
