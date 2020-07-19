{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Proto.Messages.PaxosMessages where

import qualified Data.Binary as Bn
import qualified Data.Default as Df
import qualified GHC.Generics as Gn

import qualified Proto.Common as Co

data Tablet_Entry =
  Read {
    requestId :: Co.RequestId,
    key :: String,
    timestamp :: Int } |
  Write {
    requestId :: Co.RequestId,
    key :: String,
    value :: String,
    timestamp :: Int }
  deriving (Gn.Generic, Bn.Binary, Show, Eq)

data Slave_Entry =
  RangeRead {
    requestId :: Co.RequestId,
    timestamp :: Int } |
  RangeWrite {
    requestId :: Co.RequestId,
    timestamp :: Int,
    rangeTIds :: [(Co.KeySpaceRange, Co.TabletId)] }
  deriving (Gn.Generic, Bn.Binary, Show, Eq)

data Master_Entry =
  CreateDatabase {
    requestId :: Co.RequestId,
    databaseId :: Co.DatabaseId,
    tableId :: Co.TableId,
    timestamp :: Co.Timestamp,
    eId :: Co.EndpointId,
    uid :: Co.UID } |
  DeleteDatabase {
    requestId :: Co.RequestId,
    databaseId :: Co.DatabaseId,
    tableId :: Co.TableId,
    timestamp :: Co.Timestamp,
    eId :: Co.EndpointId,
    uid :: Co.UID } |
  PickKeySpace {
    slaveGroupId :: Co.SlaveGroupId,
    choice :: Co.Choice,
    uid :: Co.UID }
  deriving (Gn.Generic, Bn.Binary, Show, Eq)

-- TODO: maybe it's better to namespace these PaxosLogEntries,
-- since we have Tablet and Slave in TraceMessages which are there
-- precisely to differentiate between different paxos logs for when
-- we reconstruct data from the trace messages for testing.
data PaxosLogEntry =
  Tablet Tablet_Entry |
  Slave Slave_Entry |
  Master Master_Entry
  deriving (Gn.Generic, Bn.Binary, Show, Eq)

type IndexT = Int
type Rnd = Int
type Val = PaxosLogEntry

data PaxosMessage =
  Propose { crnd :: Rnd, cval :: Val } |
  Prepare { crnd :: Rnd} |
  Promise { crnd :: Rnd, vrnd :: Rnd, vval :: Maybe Val } |
  Accept { crnd :: Rnd, cval :: Val } |
  Learn { lrnd :: Rnd, lval :: Val }
  deriving (Gn.Generic, Bn.Binary, Show)

data MultiPaxosMessage =
  PaxosMessage { index :: IndexT, message :: PaxosMessage }
  deriving (Gn.Generic, Bn.Binary, Show)
