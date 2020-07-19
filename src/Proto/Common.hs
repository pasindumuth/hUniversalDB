{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Proto.Common where

import qualified Data.Binary as Bn
import qualified Data.Default as Df
import qualified GHC.Generics as Gn

import Infra.Lens

type SlaveGroupId = String
type EndpointId = String

newtype UID = UID { getUid :: String } deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)
newtype PaxosId = PaxosId { getUID :: UID } deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)
newtype TabletId = TabletId { getPaxosId :: PaxosId } deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)

type RequestId = String
type DatabaseId = String
type TableId = String
type Timestamp = Int
type Lat = Timestamp
type Key = String
type Value = String

type ErrorMsg = String

data KeySpaceRange = KeySpaceRange {
  _databaseId :: DatabaseId,
  _tableId :: TableId
} deriving (Gn.Generic, Df.Default, Bn.Binary, Show, Eq, Ord)

makeLenses ''KeySpaceRange

type KeySpace = [KeySpaceRange]

data ChangingKeySpace = ChangingKeySpace {
  _oldKeySpace :: KeySpace,
  _newKeySpace :: KeySpace
} deriving (Gn.Generic, Df.Default, Bn.Binary, Show, Eq, Ord)

makeLenses ''ChangingKeySpace

data Choice =
  NewChoice |
  OldChoice
  deriving (Gn.Generic, Bn.Binary, Show, Eq)
