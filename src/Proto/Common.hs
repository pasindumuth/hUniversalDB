{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Proto.Common where

import qualified Data.Binary as Bn
import qualified Data.Default as Df
import qualified GHC.Generics as Gn

import Infra.Lens

newtype SlaveGroupId = SlaveGroupId { getSlaveGroupId :: String } deriving (Gn.Generic, Df.Default, Bn.Binary, Show, Eq, Ord)
newtype EndpointId = EndpointId { getEndpointId :: String } deriving (Gn.Generic, Df.Default, Bn.Binary, Show, Eq, Ord)

newtype UID = UID { getUID :: String } deriving (Gn.Generic, Df.Default, Bn.Binary, Show, Eq, Ord)
newtype PaxosId = PaxosId { getPaxosId :: UID } deriving (Gn.Generic, Df.Default, Bn.Binary, Show, Eq, Ord)
newtype TabletId = TabletId { getTabletId :: PaxosId } deriving (Gn.Generic, Df.Default, Bn.Binary, Show, Eq, Ord)

newtype RequestId = RequestId { getRequestId :: String } deriving (Gn.Generic, Df.Default, Bn.Binary, Show, Eq, Ord)
newtype DatabaseId = DatabaseId { getDatabaseId :: String } deriving (Gn.Generic, Df.Default, Bn.Binary, Show, Eq, Ord)
newtype TableId = TableId { getTableId :: String } deriving (Gn.Generic, Df.Default, Bn.Binary, Show, Eq, Ord)

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
