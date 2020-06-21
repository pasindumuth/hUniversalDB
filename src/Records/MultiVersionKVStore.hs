{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Records.MultiVersionKVStore where

import qualified Data.Map as Mp
import qualified Message as M

type Version = (M.Value, M.Timestamp)
type Versions = [Version] -- versions are stored in reverse order
type Lat = M.Timestamp -- Last Access Time

type MultiVersionKVStore = Mp.Map M.Key (Versions, Lat)
