{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ClientState where

import qualified Data.Map as Mp
import qualified Data.Set as St
import qualified System.Random as Rn

import qualified Infra.Utils as U
import qualified Proto.Common as Co
import qualified Proto.Messages.ClientRequests as CRq
import qualified Proto.Messages.ClientResponses as CRs
import qualified Proto.Messages.ClientResponses.RangeRead as CRsRR
import Infra.Lens
import Infra.State

data ClientState = ClientState {
  _i'slaveGroups :: Mp.Map Co.SlaveGroupId ([Co.EndpointId], (St.Set Co.KeySpaceRange)),
  _i'masterEIds :: [Co.EndpointId],
  -- TODO: We can keep track of keys we've successfully written so that
  -- we can focus on mostly reading them.
  _i'rand :: Rn.StdGen
} deriving (Show)

makeLenses ''ClientState

data RequestType =
       CreateDatabase |
       DeleteDatabase |
       RangeRead |
       RangeWrite |
       SlaveRead |
       SlaveWrite

type RequestTypeDist = Int -> RequestType

slaveDist :: RequestTypeDist
slaveDist r
  | r < 15 = RangeRead
  | r < 30 = RangeWrite
  | r < 65 = SlaveRead
  | otherwise = SlaveWrite

allRequestsDist :: RequestTypeDist
allRequestsDist r
  | r < 5 = CreateDatabase
  | r < 10 = DeleteDatabase
  | r < 20 = RangeRead
  | r < 30 = RangeWrite
  | r < 65 = SlaveRead
  | otherwise = SlaveWrite

masterDist :: RequestTypeDist
masterDist r
  | r < 30 = CreateDatabase
  | r < 65 = SlaveRead
  | otherwise = SlaveWrite

createDatabaseDist :: RequestTypeDist
createDatabaseDist _ = CreateDatabase

rangeReadDist :: RequestTypeDist
rangeReadDist _ = RangeRead

maxTable = 200
maxKey = 200
maxValue = 200

-- We randomly produce:
-- 1. Ranges: KeySpaceRanges "d" "t<n>" where 0 <= n < 1000
-- 2. Keys: "key<n>" where 0 <= n < 1000
-- 2. Values: "values<n>" where 0 <= n < 10000
genRequest
  :: Co.Timestamp
  -> RequestTypeDist
  -> STS ClientState (Co.EndpointId, CRq.Payload)
genRequest trueTimestamp requestDist = do
  slaveGroups <- getL $ i'slaveGroups
  let allRanges = St.unions (map (snd . snd) $ Mp.toList slaveGroups)
  r <- i'rand .^^ Rn.randomR (0, 99)
  case requestDist r of
    CreateDatabase -> do
      createExistingProb :: Int <- i'rand .^^ Rn.randomR (0, 99)
      Co.KeySpaceRange databaseId tableId <-
        if createExistingProb >= 80 && St.size allRanges > 0
          -- Here, we do the erronous task of trying to create a
          -- KeySpaceRange that already exists
          then i'rand .^^ U.randomS allRanges
          else makeRange
      eId <- makeMasterEId
      timestamp <- makeTimestamp
      -- TODO: we should make these tests such that using a future timestamp is unecessary.
      return (eId, CRq.CreateDatabase databaseId tableId (timestamp + 20))
    DeleteDatabase -> do
      deleteNonExistingProb :: Int <- i'rand .^^ Rn.randomR (0, 99)
      Co.KeySpaceRange databaseId tableId <-
        if deleteNonExistingProb >= 80 || St.size allRanges == 0
          -- Here, we attempt to do the erronous task of trying to delete a
          -- KeySpaceRange that doesn't exist by randomly selecting a range.
          then makeRange
          else i'rand .^^ U.randomS allRanges
      eId <- makeMasterEId
      timestamp <- makeTimestamp
      return (eId, CRq.DeleteDatabase databaseId tableId (timestamp + 20))
    RangeRead -> rangeRead
    RangeWrite -> do
      (slaveGroupId, (slaveEIds, curRanges)) <- i'rand .^^ U.randomM slaveGroups
      eId <- i'rand .^^ U.randomL slaveEIds
      addNewRange <- i'rand .^^ Rn.random
      curRanges <-
        if addNewRange
          then do
            range <- makeRange
            return $ curRanges & St.insert range
          else return $ curRanges
      timestamp <- makeTimestamp
      return (eId, CRq.RangeWrite (St.toList curRanges) timestamp)
    SlaveRead -> do
      let nonemptySlaveGroups = Mp.filter (\(_, ranges) -> St.size ranges > 0) slaveGroups
      if Mp.size nonemptySlaveGroups > 0
        then do
          (slaveGroupId, (slaveEIds, curRanges)) <- i'rand .^^ U.randomM nonemptySlaveGroups
          eId <- i'rand .^^ U.randomL slaveEIds
          pickNonExistingProb :: Int <- i'rand .^^ Rn.randomR (0, 99)
          Co.KeySpaceRange databaseId tableId <-
            if pickNonExistingProb >= 80 || St.size curRanges == 0
              -- Here, we attempt to do the erronous task of trying to read from a
              -- KeySpaceRange that doesn't exist by randomly selecting a range.
              then makeRange
              else i'rand .^^ U.randomS curRanges
          key <- makeKey
          timestamp <- makeTimestamp
          return (eId, CRq.SlaveRead databaseId tableId key timestamp)
        else rangeRead
    SlaveWrite -> do
      let nonemptySlaveGroups = Mp.filter (\(_, ranges) -> St.size ranges > 0) slaveGroups
      if Mp.size nonemptySlaveGroups > 0
        then do
          (slaveGroupId, (slaveEIds, curRanges)) <- i'rand .^^ U.randomM nonemptySlaveGroups
          eId <- i'rand .^^ U.randomL slaveEIds
          pickNonExistingProb :: Int <- i'rand .^^ Rn.randomR (0, 99)
          Co.KeySpaceRange databaseId tableId <-
            if pickNonExistingProb >= 80 || St.size curRanges == 0
              -- Here, we attempt to do the erronous task of trying to write to a
              -- KeySpaceRange that doesn't exist by randomly selecting a range.
              then makeRange
              else i'rand .^^ U.randomS curRanges
          key <- makeKey
          value <- makeValue
          timestamp <- makeTimestamp
          return (eId, CRq.SlaveWrite databaseId tableId key value timestamp)
        else rangeRead
  where
    makeMasterEId = do
      masterEIds <- getL $ i'masterEIds
      i'rand .^^ U.randomL masterEIds
    makeTimestamp = do
      noise <- i'rand .^^ Rn.randomR (-2, 2)
      return $ trueTimestamp + noise
    makeRange = do
      i :: Int <- i'rand .^^ Rn.randomR (0, maxTable)
      return $ Co.KeySpaceRange (Co.DatabaseId "d") (Co.TableId $ "t" ++ show i)
    makeKey = do
      i :: Int <- i'rand .^^ Rn.randomR (0, maxKey)
      return $ "k" ++ show i
    makeValue = do
      i :: Int <- i'rand .^^ Rn.randomR (0, maxValue)
      return $ "v" ++ show i
    rangeRead = do
      slaveGroups <- getL $ i'slaveGroups
      (slaveGroupId, (slaveEIds, _)) <- i'rand .^^ U.randomM slaveGroups
      eId <- i'rand .^^ U.randomL slaveEIds
      timestamp <- makeTimestamp
      return (eId, CRq.RangeRead timestamp)

handleResponse
  :: Co.EndpointId
  -> CRs.ClientResponse
  -> STS ClientState ()
handleResponse fromEId response = do
  case response ^. CRs.payload of
    CRs.RangeRead (CRsRR.Success ranges) -> do
      i'slaveGroups .^^. \slaveGroups ->
        U.s12 Mp.map slaveGroups $
          \(slaveEIds, curRanges) ->
            if elem fromEId slaveEIds
              then (slaveEIds, St.fromList ranges)
              else (slaveEIds, curRanges)
      return ()
    _ -> return ()
