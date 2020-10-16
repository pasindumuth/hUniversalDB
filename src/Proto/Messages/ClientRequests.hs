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

data Meta = Meta {
  _requestId :: Co.RequestId
} deriving (Gn.Generic, Bn.Binary, Show)

data Payload =
  RangeRead {
    timestamp :: Int } |
  RangeWrite {
    ranges :: [Co.KeySpaceRange],
    timestamp :: Int } |
  SlaveRead {
    path :: Co.Path,
    key :: String,
    timestamp :: Int } |
  SlaveWrite {
    path :: Co.Path,
    key :: String,
    value :: String,
    timestamp :: Int } |
  CreateDatabase {
    path :: Co.Path,
    timestamp :: Co.Timestamp } |
  DeleteDatabase {
    path :: Co.Path,
    timestamp :: Co.Timestamp }
  deriving (Gn.Generic, Bn.Binary, Show)

data ClientRequest = ClientRequest {
  _meta :: Meta,
  _payload :: Payload
} deriving (Gn.Generic, Bn.Binary, Show)

makeLenses ''Meta
makeLenses '' ClientRequest
