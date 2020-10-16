module Slave.Tablet.MultiVersionKVStore (
  MultiVersionKVStore,
  MVM.read,
  MVM.staticRead,
  MVM.staticReadLat,
  MVM.write,
) where

import qualified Common.MultiVersionMap as MVM
import qualified Proto.Common as Co

type MultiVersionKVStore = MVM.MultiVersionMap Co.Key (Co.Value, Co.RequestId) ()
