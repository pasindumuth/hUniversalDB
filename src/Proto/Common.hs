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

-- This globally identifies a table of a database in a whole deployment of hUniversalDB.
-- Note that this means that Path encompasses both the table name and the database name.
newtype Path = Path { getPath :: String } deriving (Gn.Generic, Df.Default, Bn.Binary, Show, Eq, Ord)

-- This represents a subset of keys: [startKey, endKey). If startKey/endKey
-- are Nothing, there is no lower/upper bound.
data KeyRange = KeyRange {
  _startKey :: Maybe String,
  _endKey :: Maybe String
} deriving (Gn.Generic, Df.Default, Bn.Binary, Show, Eq, Ord)

-- This globally identifies a partition of a table of a database at a given
-- moment in time in a whole deployment of hUniversalDB. Recall that a
-- deployment is a database cluster.
data PartitionShape = PartitionShape {
  _path :: Path,
  _range :: KeyRange
} deriving (Gn.Generic, Df.Default, Bn.Binary, Show, Eq, Ord)

type Timestamp = Int
type Lat = Timestamp
type Key = String
type Value = String

type ErrorMsg = String

-- @Deprecated
data KeySpaceRange = KeySpaceRange {
  _path :: Path
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
