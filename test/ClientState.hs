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
  _i'curRanges :: St.Set Co.KeySpaceRange,
  _i'slaveEIds :: [Co.EndpointId],
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
  | r < 10 = CreateDatabase
  | r < 20 = RangeRead
  | r < 30 = RangeWrite
  | r < 65 = SlaveRead
  | otherwise = SlaveWrite

masterDist :: RequestTypeDist
masterDist r
  | r < 30 = CreateDatabase
  | r < 65 = SlaveRead
  | otherwise = SlaveWrite

maxTable = 1000
maxKey = 1000
maxValue = 1000

-- We randomly produce:
-- 1. Ranges: KeySpaceRanges "d" "t<n>" where 0 <= n < 1000
-- 2. Keys: "key<n>" where 0 <= n < 1000
-- 2. Values: "values<n>" where 0 <= n < 10000
genRequest
  :: Co.Timestamp
  -> RequestTypeDist
  -> STS ClientState (Co.EndpointId, CRq.Payload)
genRequest trueTimestamp requestDist = do
  curRanges <- getL $ i'curRanges
  r <- i'rand .^^ Rn.randomR (0, 99)
  case requestDist r of
    CreateDatabase -> do
      createExistingProb :: Int <- i'rand .^^ Rn.randomR (0, 99)
      Co.KeySpaceRange databaseId tableId <-
        if createExistingProb >= 80 && St.size curRanges > 0
          -- Here, we do the erronous task of trying to create a
          -- KeySpaceRange that already exists
          then i'rand .^^ U.randomS curRanges
          else makeRange
      eId <- makeMasterEId
      timestamp <- makeTimestamp
      return (eId, CRq.CreateDatabase databaseId tableId timestamp)
    DeleteDatabase -> do
      deleteNonExistingProb :: Int <- i'rand .^^ Rn.randomR (0, 99)
      Co.KeySpaceRange databaseId tableId <-
        if deleteNonExistingProb >= 80 || St.size curRanges > 0
          -- Here, we attempt to do the erronous task of trying to delete a
          -- KeySpaceRange that doesn't exist by randomly selecting a range.
          then makeRange
          else i'rand .^^ U.randomS curRanges
      eId <- makeMasterEId
      timestamp <- makeTimestamp
      return (eId, CRq.DeleteDatabase databaseId tableId timestamp)
    RangeRead -> do
      eId <- makeSlaveEId
      timestamp <- makeTimestamp
      return (eId, CRq.RangeRead timestamp)
    RangeWrite -> do
      addNewRange <- i'rand .^^ Rn.random
      curRanges <- if addNewRange
                      then do
                        range <- makeRange
                        return $ curRanges & St.insert range
                      else return $ curRanges
      eId <- makeSlaveEId
      timestamp <- makeTimestamp
      return (eId, CRq.RangeWrite (St.toList curRanges) timestamp)
    SlaveRead -> do
      pickNonExistingProb :: Int <- i'rand .^^ Rn.randomR (0, 99)
      Co.KeySpaceRange databaseId tableId <-
        if pickNonExistingProb >= 80 || St.size curRanges > 0
          -- Here, we attempt to do the erronous task of trying to read from a
          -- KeySpaceRange that doesn't exist by randomly selecting a range.
          then makeRange
          else i'rand .^^ U.randomS curRanges
      key <- makeKey
      eId <- makeSlaveEId
      timestamp <- makeTimestamp
      return (eId, CRq.SlaveRead databaseId tableId key timestamp)
    SlaveWrite -> do
      pickNonExistingProb :: Int <- i'rand .^^ Rn.randomR (0, 99)
      Co.KeySpaceRange databaseId tableId <-
        if pickNonExistingProb >= 80 || St.size curRanges > 0
          -- Here, we attempt to do the erronous task of trying to write to a
          -- KeySpaceRange that doesn't exist by randomly selecting a range.
          then makeRange
          else i'rand .^^ U.randomS curRanges
      key <- makeKey
      value <- makeValue
      eId <- makeSlaveEId
      timestamp <- makeTimestamp
      return (eId, CRq.SlaveWrite databaseId tableId key value timestamp)
  where
    makeMasterEId = do
      masterEIds <- getL $ i'masterEIds
      eId <- i'rand .^^ U.randomL masterEIds
      return eId
    makeSlaveEId = do
      slaveEIds <- getL $ i'slaveEIds
      eId <- i'rand .^^ U.randomL slaveEIds
      return eId
    makeTimestamp = do
      noise <- i'rand .^^ Rn.randomR (-2, 2)
      return $ trueTimestamp + noise
    makeRange = do
      i :: Int <- i'rand .^^ Rn.randomR (0, maxTable)
      return $ Co.KeySpaceRange "d" ("t" ++ show i)
    makeKey = do
      i :: Int <- i'rand .^^ Rn.randomR (0, maxKey)
      return $ "k" ++ show i
    makeValue = do
      i :: Int <- i'rand .^^ Rn.randomR (0, maxValue)
      return $ "v" ++ show i

handleResponse
  :: CRs.ClientResponse
  -> STS ClientState ()
handleResponse response = do
  case response ^. CRs.payload of
    CRs.RangeRead (CRsRR.Success ranges) -> do
      i'curRanges .^^. \_ -> St.fromList ranges
      return ()
    _ -> return ()
