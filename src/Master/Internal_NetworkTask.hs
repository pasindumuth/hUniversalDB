{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Master.Internal_NetworkTask where

import qualified Data.Default as Df
import qualified Data.Map as Mp
import qualified GHC.Generics as Gn

import qualified Proto.Common as Co
import Infra.Lens

data NetworkTask = NetworkTask {
  eId :: Co.EndpointId,
  requestId :: Co.RequestId,
  timestamp :: Co.Timestamp,
  slaveGroupId :: Co.SlaveGroupId
} deriving (Gn.Generic, Df.Default, Show)

makeLenses ''NetworkTask
