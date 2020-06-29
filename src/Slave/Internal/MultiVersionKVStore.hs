{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Slave.Internal.MultiVersionKVStore where

import qualified Data.Map as Mp

type Key = String
type Value = String
type Timestamp = Int

type Version = (Value, Timestamp)
type Versions = [Version] -- versions are stored in reverse order
type Lat = Timestamp -- Last Access Time

type MultiVersionKVStore = Mp.Map Key (Versions, Lat)
