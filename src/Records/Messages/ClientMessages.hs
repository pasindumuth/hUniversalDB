{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Records.Messages.ClientMessages where

import qualified Data.Binary as B
import qualified Data.Default as D
import qualified GHC.Generics as G

data ClientRequest =
  ReadRequest { key :: String, timestamp :: Int } |
  WriteRequest { key :: String, value :: String, timestamp :: Int }
  deriving (G.Generic, B.Binary, Show)

data ClientResponse =
  Error { message :: String } |
  ReadResponse { value :: Maybe String } |
  WritResponse
  deriving (G.Generic, B.Binary, Show)
