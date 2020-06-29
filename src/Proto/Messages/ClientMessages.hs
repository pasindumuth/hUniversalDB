{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Proto.Messages.ClientMessages where

import qualified Data.Binary as Bn
import qualified Data.Default as Df
import qualified GHC.Generics as Gn

data ClientRequest =
  ReadRequest { key :: String, timestamp :: Int } |
  WriteRequest { key :: String, value :: String, timestamp :: Int }
  deriving (Gn.Generic, Bn.Binary, Show)

data ClientResponse =
  Error { message :: String } |
  ReadResponse { value :: Maybe String } |
  WriteResponse
  deriving (Gn.Generic, Bn.Binary, Show)
