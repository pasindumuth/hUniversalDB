{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module RequestStats where

import qualified Data.Default as Df
import qualified GHC.Generics as Gn

import qualified Proto.Messages.ClientRequests as CRq
import qualified Proto.Messages.ClientResponses as CRs
import qualified Proto.Messages.ClientResponses.CreateDatabase as CRsCD
import qualified Proto.Messages.ClientResponses.DeleteDatabase as CRsDD
import qualified Proto.Messages.ClientResponses.RangeRead as CRsRR
import qualified Proto.Messages.ClientResponses.RangeWrite as CRsRW
import qualified Proto.Messages.ClientResponses.SlaveRead as CRsSR
import qualified Proto.Messages.ClientResponses.SlaveWrite as CRsSW
import Infra.Lens
import Infra.State

data CreateDatabaseStats = CreateDatabaseStats {
  _numCreateDatabaseRqs :: Int,
  _numCreateDatabaseBackwardsWriteMasterRss :: Int,
  _numCreateDatabaseBackwardsWriteSlaveRss :: Int,
  _numCreateDatabaseAlreadyExistsRss :: Int,
  _numCreateDatabaseNothingChangedRss :: Int,
  _numCreateDatabaseSuccessRss :: Int
} deriving (Gn.Generic, Df.Default, Show)

data DeleteDatabaseStats = DeleteDatabaseStats {
  _numDeleteDatabaseRqs :: Int,
  _numDeleteDatabaseBackwardsWriteMasterRss :: Int,
  _numDeleteDatabaseBackwardsWriteSlaveRss :: Int,
  _numDeleteDatabaseDoesNotExistsRss :: Int,
  _numDeleteDatabaseNothingChangedRss :: Int,
  _numDeleteDatabaseSuccessRss :: Int
} deriving (Gn.Generic, Df.Default, Show)

data RangeReadStats = RangeReadStats {
  _numRangeReadRqs :: Int,
  _numRangeReadSuccessRss :: Int
} deriving (Gn.Generic, Df.Default, Show)

data RangeWriteStats = RangeWriteStats {
  _numRangeWriteRqs :: Int,
  _numRangeWriteBackwardsWriteRss :: Int,
  _numRangeWriteSuccessRss :: Int
} deriving (Gn.Generic, Df.Default, Show)

data SlaveReadStats = SlaveReadStats {
  _numSlaveReadRqs :: Int,
  _numSlaveReadUnknownDBRss :: Int,
  _numSlaveReadSuccessRss :: Int
} deriving (Gn.Generic, Df.Default, Show)

data SlaveWriteStats = SlaveWriteStats {
  _numSlaveWriteRqs :: Int,
  _numSlaveWriteUnknownDBRss :: Int,
  _numBackwardsWriteRss :: Int,
  _numSlaveWriteSuccessRss :: Int
} deriving (Gn.Generic, Df.Default, Show)

data RequestStats = RequestStats {
  _createDatabaseStats :: CreateDatabaseStats,
  _deleteDatabaseStats :: DeleteDatabaseStats,
  _rangeReadStats :: RangeReadStats,
  _rangeWriteStats :: RangeWriteStats,
  _slaveReadStats :: SlaveReadStats,
  _slaveWriteStats :: SlaveWriteStats
} deriving (Gn.Generic, Df.Default, Show)

makeLenses ''CreateDatabaseStats
makeLenses ''DeleteDatabaseStats
makeLenses ''RangeReadStats
makeLenses ''RangeWriteStats
makeLenses ''SlaveReadStats
makeLenses ''SlaveWriteStats
makeLenses ''RequestStats

recordResponse :: CRs.Payload -> STS RequestStats ()
recordResponse payload = do
  case payload of
    CRs.CreateDatabase CRsCD.BackwardsWriteMaster -> createDatabaseStats . numCreateDatabaseBackwardsWriteMasterRss .^^. (+1)
    CRs.CreateDatabase CRsCD.BackwardsWriteSlave -> createDatabaseStats . numCreateDatabaseBackwardsWriteSlaveRss .^^. (+1)
    CRs.CreateDatabase CRsCD.AlreadyExists -> createDatabaseStats . numCreateDatabaseAlreadyExistsRss .^^. (+1)
    CRs.CreateDatabase CRsCD.NothingChanged -> createDatabaseStats . numCreateDatabaseNothingChangedRss .^^. (+1)
    CRs.CreateDatabase CRsCD.Success -> createDatabaseStats . numCreateDatabaseSuccessRss .^^. (+1)
    CRs.DeleteDatabase CRsDD.BackwardsWriteMaster -> deleteDatabaseStats . numDeleteDatabaseBackwardsWriteMasterRss .^^. (+1)
    CRs.DeleteDatabase CRsDD.BackwardsWriteSlave -> deleteDatabaseStats . numDeleteDatabaseBackwardsWriteSlaveRss .^^. (+1)
    CRs.DeleteDatabase CRsDD.DoesNotExist -> deleteDatabaseStats . numDeleteDatabaseDoesNotExistsRss .^^. (+1)
    CRs.DeleteDatabase CRsDD.NothingChanged -> deleteDatabaseStats . numDeleteDatabaseNothingChangedRss .^^. (+1)
    CRs.DeleteDatabase CRsDD.Success -> deleteDatabaseStats . numDeleteDatabaseSuccessRss .^^. (+1)
    CRs.RangeRead (CRsRR.Success _) -> rangeReadStats . numRangeReadSuccessRss .^^. (+1)
    CRs.RangeWrite CRsRW.BackwardsWrite -> rangeWriteStats . numRangeWriteBackwardsWriteRss .^^. (+1)
    CRs.RangeWrite CRsRW.Success -> rangeWriteStats . numRangeWriteSuccessRss .^^. (+1)
    CRs.SlaveRead CRsSR.UnknownDB -> slaveReadStats . numSlaveReadUnknownDBRss .^^. (+1)
    CRs.SlaveRead (CRsSR.Success _) -> slaveReadStats . numSlaveReadSuccessRss .^^. (+1)
    CRs.SlaveWrite CRsSW.UnknownDB -> slaveWriteStats . numSlaveWriteUnknownDBRss .^^. (+1)
    CRs.SlaveWrite CRsSW.BackwardsWrite -> slaveWriteStats . numBackwardsWriteRss .^^. (+1)
    CRs.SlaveWrite CRsSW.Success -> slaveWriteStats . numSlaveWriteSuccessRss .^^. (+1)
  return ()

recordRequest :: CRq.Payload -> STS RequestStats ()
recordRequest payload = do
  case payload of
    CRq.CreateDatabase _ _ _ -> createDatabaseStats . numCreateDatabaseRqs .^^. (+1)
    CRq.DeleteDatabase _ _ _ -> deleteDatabaseStats . numDeleteDatabaseRqs .^^. (+1)
    CRq.RangeRead _ -> rangeReadStats . numRangeReadRqs .^^. (+1)
    CRq.RangeWrite _ _ -> rangeWriteStats . numRangeWriteRqs .^^. (+1)
    CRq.SlaveRead _ _ _ _ -> slaveReadStats . numSlaveReadRqs .^^. (+1)
    CRq.SlaveWrite _ _ _ _ _ -> slaveWriteStats . numSlaveWriteRqs .^^. (+1)
  return ()

