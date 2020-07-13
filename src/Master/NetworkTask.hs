{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Master.NetworkTask (
  NetworkTask
) where

import qualified Data.Default as Df
import qualified Data.Map as Mp
import qualified GHC.Generics as Gn

import qualified Proto.Common as Co
import Infra.Lens

data NetworkTask = NetworkTask {
  _i'eId :: Co.EndpointId,
  _i'requestId :: Co.RequestId,
  _i'timestamp :: Co.Timestamp,
  _i'slaveGroupId :: Co.SlaveGroupId
} deriving (Gn.Generic, Df.Default, Show)

makeLenses ''NetworkTask
