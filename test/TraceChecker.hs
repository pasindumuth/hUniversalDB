{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module TraceChecker (
  refineTrace,
  checkMsgs
) where

import qualified Control.Monad as Mo
import qualified Data.Default as Df
import qualified Data.List as Li
import qualified Data.Map as Mp
import qualified Data.Set as St
import qualified GHC.Generics as Gn

import qualified Infra.Utils as U
import qualified Proto.Messages.ClientRequests as CRq
import qualified Proto.Messages.ClientResponses as CRs
import qualified Proto.Messages.ClientResponses.RangeRead as CRsRR
import qualified Proto.Messages.ClientResponses.RangeWrite as CRsRW
import qualified Proto.Messages.ClientResponses.SlaveRead as CRsSR
import qualified Proto.Messages.ClientResponses.SlaveWrite as CRsSW
import qualified Proto.Messages.PaxosMessages as PM
import qualified Proto.Messages.TraceMessages as TrM
import qualified Proto.Common as Co
import qualified Slave.KeySpaceManager as KSM
import qualified Slave.Tablet.MultiVersionKVStore as MVS
import Infra.Lens
import Infra.State

data CheckState = CheckState {
  _i'requestMap :: Mp.Map Co.RequestId CRq.Payload,
  _i'rangeMap :: Mp.Map Co.TabletId Co.KeySpaceRange,
  _i'tables :: Mp.Map Co.TabletId MVS.MultiVersionKVStore,
  _i'keySpaceManager :: KSM.KeySpaceManager
} deriving (Gn.Generic, Df.Default)

data TestPaxosLog = TestPaxosLog {
  _i'plog :: Mp.Map PM.IndexT PM.PaxosLogEntry,
  _i'nextIdx :: PM.IndexT
}

makeLenses ''CheckState
makeLenses ''TestPaxosLog

addMsgs
 :: Co.PaxosId
 -> PM.IndexT
 -> Mp.Map PM.IndexT PM.PaxosLogEntry
 -> [TrM.TraceMessage]
 -> (PM.IndexT, [TrM.TraceMessage])
addMsgs paxosId i m msgs =
  case Mp.lookup i m of
    Just v -> addMsgs paxosId (i + 1) m (TrM.PaxosInsertion paxosId i v : msgs)
    Nothing -> (i, msgs)

-- This functions restructures the messages so that PaxosInsertions occur in
-- order of their index, where the occur as early as they are first seen. This function
-- also checks if the PaxosLogInsertion are consistent (i.e. the entry for a PaxosLogInsertion
-- at a given PaxosId and Index are the same).
refineTrace :: [TrM.TraceMessage] -> Either Co.ErrorMsg [TrM.TraceMessage]
refineTrace msgs =
  let paxosLogsE = U.foldM (Mp.empty, []) msgs $
        \(paxosLogs, modMsgs) msg ->
          case msg of
            TrM.PaxosInsertion paxosId index entry ->
              case paxosLogs ^. at paxosId of
                Just paxosLog ->
                  case paxosLog ^. i'plog . at index of
                    Just entry' ->
                      if entry' == entry
                        then Right (paxosLogs, modMsgs)
                        else Left $ "A PaxosLog entry mismatch occurred at: " ++
                                    "PaxosId = " ++ show paxosId ++
                                    ", Entry: (" ++ show entry ++ ") vs. (" ++ show entry' ++ ")"
                    _ ->
                      let plog' = paxosLog ^. i'plog & at index ?~ entry
                          (nextIdx', modMsgs') = addMsgs paxosId (paxosLog ^. i'nextIdx) plog' modMsgs
                          paxosLog' = TestPaxosLog plog' nextIdx'
                          paxosLogs' = paxosLogs & at paxosId ?~ paxosLog'
                      in Right (paxosLogs', modMsgs')
                Nothing ->
                  let plog' = Mp.empty & at index ?~ entry
                      (nextIdx', modMsgs') = addMsgs paxosId 0 plog' modMsgs
                      paxosLog' = TestPaxosLog plog' nextIdx'
                      paxosLogs' = paxosLogs & at paxosId ?~ paxosLog'
                  in Right (paxosLogs', modMsgs')
            _ -> Right (paxosLogs, (msg:modMsgs))
  in case paxosLogsE of
    Right (_, modMsgs) -> Right $ reverse modMsgs
    Left errMsg -> Left errMsg

checkMsgs :: [TrM.TraceMessage] -> Either Co.ErrorMsg ()
checkMsgs msgs =
  let ret = U.foldM (Df.def :: CheckState) msgs $ \state msg ->
              let (ret, (_, _, state')) = runST (checkMsg msg) state
              in case ret of
                Left message -> Left message
                Right _ -> Right state'
  in case ret of
    Left message -> Left message
    Right state ->
      if (state ^. i'requestMap & Mp.size) > 0
        then Left $ "The following requests went unanswered: " ++ ppShow (state ^. i'requestMap)
        else Right ()

entryUnfamiliarE :: PM.PaxosLogEntry -> Either Co.ErrorMsg a
entryUnfamiliarE entry =
  Left $ "PaxosLogEntry (" ++ ppShow entry ++
         ") has unfamiliar requestId."

entryIncorrectE :: PM.PaxosLogEntry -> CRq.Payload -> Either Co.ErrorMsg a
entryIncorrectE entry payload =
  Left $ "PaxosLogEntry (" ++ ppShow entry ++
         ") isn't correct. Request payload was (" ++ ppShow payload

responseNoRequestE :: CRs.ClientResponse -> Either Co.ErrorMsg a
responseNoRequestE response =
  Left $ "Response (" ++ ppShow response ++
         ") has no corresponding Request"

responseTypeIncorrectE :: CRs.ClientResponse -> CRq.Payload -> Either Co.ErrorMsg a
responseTypeIncorrectE response requestPayload =
  Left $ "Response (" ++ ppShow response ++
         ") has the wrong type for Request (" ++ ppShow requestPayload

responseValueIncorrectE :: CRs.ClientResponse -> CRq.Payload -> Either Co.ErrorMsg a
responseValueIncorrectE response requestPayload =
  Left $ "Response (" ++ ppShow response ++
         ") has the wrong value for Request (" ++ ppShow requestPayload

derivedStateIncorrectlyWrittenE :: CRq.Payload -> CRs.ClientResponse -> Either Co.ErrorMsg a
derivedStateIncorrectlyWrittenE requestPayload response =
  Left $ "Derived State didn't appear to reflect changes in the Request (" ++ ppShow requestPayload ++
         ") at the time of response (" ++ ppShow response

-- Notice that we often use fatally failing operations, like ^?!. This is because
-- if the program is working right, then these operations shouldn't fail.
-- Rather than checking whether the oepration would fail or not by using
-- safe operations, it's more compact to just fail fatally.
-- TODO: maybe change all of these to assertions
-- TODO: We must revamp this so that it works with Master
checkMsg :: TrM.TraceMessage -> STS CheckState (Either Co.ErrorMsg ())
checkMsg msg = do
  case msg of
    TrM.PaxosInsertion paxosId _ paxosLogEntry ->
      case paxosLogEntry of
        PM.Tablet entry -> do
          let tabletId = (Co.TabletId paxosId)
          range <- getT $ i'rangeMap . ix tabletId
          case entry of
            PM.Read requestId key timestamp -> do
              payloadM <- getL $ i'requestMap . at requestId
              case payloadM of
                Nothing -> return $ entryUnfamiliarE paxosLogEntry
                Just payload ->
                  case payload of
                    CRq.SlaveRead path' key' timestamp'
                      | (Co.KeySpaceRange path') == range &&
                        key' == key &&
                        timestamp' == timestamp -> do
                      i'tables . ix tabletId .^^* MVS.read key timestamp
                      return $ Right ()
                    _ -> return $ entryIncorrectE paxosLogEntry payload
            PM.Write requestId key value timestamp -> do
              payloadM <- getL $ i'requestMap . at requestId
              case payloadM of
                Nothing -> return $ entryUnfamiliarE paxosLogEntry
                Just payload ->
                  case payload of
                    CRq.SlaveWrite path' key' value' timestamp'
                      | (Co.KeySpaceRange path') == range &&
                        key' == key &&
                        value' == value &&
                        timestamp' == timestamp -> do
                      i'tables . ix tabletId .^^* MVS.write key (value, requestId) timestamp
                      return $ Right ()
                    _ -> return $ entryIncorrectE paxosLogEntry payload
        PM.Slave entry ->
          case entry of
            PM.RangeRead requestId timestamp -> do
              payloadM <- getL $ i'requestMap . at requestId
              case payloadM of
                Nothing -> return $ entryUnfamiliarE paxosLogEntry
                Just payload -> do
                  let entryCorrect =
                        case payload of
                        CRq.RangeRead timestamp'
                          | timestamp' == timestamp -> True
                        CRq.SlaveRead _ _ timestamp'
                          | timestamp' == timestamp -> True
                        CRq.SlaveWrite _ _ _ timestamp'
                          | timestamp' == timestamp -> True
                        _ -> False
                  if entryCorrect
                    then do
                      i'keySpaceManager .^^ KSM.read timestamp
                      return $ Right ()
                    else return $ entryIncorrectE paxosLogEntry payload
            PM.RangeWrite requestId timestamp rangeTIds -> do
              let ranges = map fst rangeTIds
              payloadM <- getL $ i'requestMap . at requestId
              case payloadM of
                Nothing -> return $ entryUnfamiliarE paxosLogEntry
                Just payload -> do
                  case payload of
                    -- TODO: make sure that the UIDs of the KeySpaceRanges common to the
                    -- new ranges as well as the old are still the same.
                    CRq.RangeWrite ranges' timestamp'
                      | (St.fromList ranges') == (St.fromList ranges) &&
                        timestamp' == timestamp -> do
                      i'keySpaceManager .^^ KSM.write timestamp requestId rangeTIds
                      -- Update the Checkstate with the new ranges
                      Mo.forM rangeTIds $ \(range, tabletId) -> do
                        hasRange <- i'rangeMap .^^^ Mp.member tabletId
                        if hasRange
                          then return ()
                          else do
                            i'rangeMap .^^. Mp.insert tabletId range
                            i'tables .^^. Mp.insert tabletId Df.def
                            return ()
                      return $ Right ()
                    _ -> return $ entryIncorrectE paxosLogEntry payload
        -- TODO: Don't do this
        _ -> return $ Right()
    TrM.ClientRequestReceived request -> do
      i'requestMap .^^. at (request ^. CRq.meta . CRq.requestId) ?~ (request ^. CRq.payload)
      return $ Right ()
    TrM.ClientResponseSent response -> do
      let requestId = response ^. CRs.meta . CRs.requestId
      requestPayloadM <- getL $ i'requestMap . at requestId
      case requestPayloadM of
        Nothing -> return $ responseNoRequestE response
        Just requestPayload -> do
          i'requestMap .^^. Mp.delete requestId
          case response ^. CRs.payload of
            CRs.RangeRead responsePayload -> do
              case requestPayload of
                CRq.RangeRead timestamp -> do
                  -- staticRead ensure the `lat` is beyond `timestamp`
                  res <- i'keySpaceManager .^^^ KSM.staticRead timestamp
                  let rangeTIds = case res of
                                 Nothing -> []
                                 Just (_, _, rangeTIds) -> rangeTIds
                  if (CRsRR.Success (map fst rangeTIds)) == responsePayload
                    then return $ Right ()
                    else return $ responseValueIncorrectE response requestPayload
                _ -> return $ responseTypeIncorrectE response requestPayload
            CRs.RangeWrite responsePayload -> do
              case requestPayload of
                CRq.RangeWrite ranges timestamp -> do
                  -- staticRead ensure the `lat` is beyond `timestamp`
                  res <- i'keySpaceManager .^^^ KSM.staticRead timestamp
                  case res of
                    Just (timestamp', requestId', rangeTIds')
                      | requestId' == requestId -> do
                      case responsePayload of
                        CRsRW.Success
                          | (map fst rangeTIds') == ranges &&
                            timestamp' == timestamp -> do
                          return $ Right ()
                        _ -> return $ derivedStateIncorrectlyWrittenE requestPayload response
                    Just _
                      | responsePayload == CRsRW.BackwardsWrite -> return $ Right ()
                    _ -> return $ derivedStateIncorrectlyWrittenE requestPayload response
                _ -> return $ responseTypeIncorrectE response requestPayload
            CRs.SlaveRead responsePayload -> do
              case requestPayload of
                CRq.SlaveRead path key timestamp -> do
                  tabletIdM <- tabletIdM path timestamp
                  case tabletIdM of
                    Just tabletId -> do
                      -- staticRead ensure the `lat` is beyond `timestamp`
                      res <- i'tables . ix tabletId .^^^* MVS.staticRead key timestamp
                      let value = case res of
                                    Nothing -> Nothing
                                    Just ((value', _), _) -> Just value'
                      if (CRsSR.Success value) == responsePayload
                        then return $ Right ()
                        else return $ responseValueIncorrectE response requestPayload
                    _ | responsePayload == CRsSR.UnknownDB -> return $ Right ()
                    _ -> return $ responseValueIncorrectE response requestPayload
                _ -> return $ responseTypeIncorrectE response requestPayload
            CRs.SlaveWrite responsePayload -> do
              case requestPayload of
                CRq.SlaveWrite path key value timestamp -> do
                  tabletIdM <- tabletIdM path timestamp
                  case tabletIdM of
                    Just tabletId -> do
                      -- staticRead ensure the `lat` is beyond `timestamp`
                      res <- i'tables . ix tabletId .^^^* MVS.staticRead key timestamp
                      case res of
                        Just ((value', requestId'), timestamp')
                          | requestId' == requestId -> do
                          case responsePayload of
                            CRsSW.Success
                              | value' == value &&
                                timestamp' == timestamp ->
                              return $ Right()
                            _ -> return $ derivedStateIncorrectlyWrittenE requestPayload response
                        Just _
                          | responsePayload == CRsSW.BackwardsWrite -> return $ Right ()
                        _ -> return $ derivedStateIncorrectlyWrittenE requestPayload response
                    _ | responsePayload == CRsSW.UnknownDB -> return $ Right ()
                    _ -> return $ responseValueIncorrectE response requestPayload
                _ -> return $ responseTypeIncorrectE response requestPayload
            -- TODO: Don't do this
            _ -> return $ Right()
      return $ Right ()
  where
    tabletIdM path timestamp = do
      res <- i'keySpaceManager .^^^ KSM.staticRead timestamp
      case res of
        Just (_, _, rangeTIds') ->
          let range = Co.KeySpaceRange path
          in case Li.find (\(range', _) -> range' == range) rangeTIds' of
            Just (_, tabletId) -> return $ Just tabletId
            _ -> return $ Nothing
        _ -> return $ Nothing