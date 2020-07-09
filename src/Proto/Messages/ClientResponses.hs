{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Proto.Messages.ClientResponses where

import qualified Data.Binary as Bn
import qualified GHC.Generics as Gn

import qualified Proto.Common as Co
import qualified Proto.Messages.ClientResponses.SlaveRead as CRsSR
import qualified Proto.Messages.ClientResponses.SlaveWrite as CRsSW
import Infra.Lens

data Meta = Meta {
  _requestId :: Co.RequestId
} deriving (Gn.Generic, Bn.Binary, Show)

data CreateDatabase =
  CreateDBSuccess
  deriving (Gn.Generic, Bn.Binary, Show)

data Payload =
  SlaveRead CRsSR.SlaveRead |
  SlaveWrite CRsSW.SlaveWrite |
  CreateDatabase CreateDatabase
  deriving (Gn.Generic, Bn.Binary, Show)

data ClientResponse = ClientResponse {
  _meta :: Meta,
  _payload :: Payload
} deriving (Gn.Generic, Bn.Binary, Show)

makeLenses ''Meta
makeLenses '' ClientResponse
