{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Slave.Tablet.Internal_MultiVersionKVStore where

import qualified Data.Map as Mp

import qualified Proto.Common as Co

type MultiVersionKVStore = Mp.Map Co.Key ([(Co.Value, Co.RequestId, Co.Timestamp)], Co.Lat)
