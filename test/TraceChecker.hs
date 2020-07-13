module TraceChecker where

import qualified Control.Monad as Mo
import qualified Data.Default as Df
import qualified Data.List as Li
import qualified Data.Map as Mp
import qualified Data.Set as St

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
import qualified Internal_TraceChecker as ITC
import Infra.Lens
import Infra.State

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
  let paxosLogsE = U.s31 Mo.foldM (Mp.empty, []) msgs $
        \(paxosLogs, modMsgs) msg ->
          case msg of
            TrM.PaxosInsertion paxosId index entry ->
              case paxosLogs ^. at paxosId of
                Just paxosLog ->
                  case paxosLog ^. ITC.plog . at index of
                    Just entry' ->
                      if entry' == entry
                        then Right (paxosLogs, modMsgs)
                        else Left $ "A PaxosLog entry mismatch occurred at: " ++
                                    "PaxosId = " ++ show paxosId ++
                                    ", Entry: (" ++ show entry ++ ") vs. (" ++ show entry' ++ ")"
                    _ ->
                      let plog' = paxosLog ^. ITC.plog & at index ?~ entry
                          (nextIdx', modMsgs') = addMsgs paxosId (paxosLog ^. ITC.nextIdx) plog' modMsgs
                          paxosLog' = ITC.TestPaxosLog plog' nextIdx'
                          paxosLogs' = paxosLogs & at paxosId ?~ paxosLog'
                      in Right (paxosLogs', modMsgs')
                Nothing ->
                  let plog' = Mp.empty & at index ?~ entry
                      (nextIdx', modMsgs') = addMsgs paxosId 0 plog' modMsgs
                      paxosLog' = ITC.TestPaxosLog plog' nextIdx'
                      paxosLogs' = paxosLogs & at paxosId ?~ paxosLog'
                  in Right (paxosLogs', modMsgs')
            _ -> Right (paxosLogs, (msg:modMsgs))
  in case paxosLogsE of
    Right (_, modMsgs) -> Right $ reverse modMsgs
    Left errMsg -> Left errMsg

checkMsgs :: [TrM.TraceMessage] -> Either Co.ErrorMsg ()
checkMsgs msgs =
  let ret = U.s31 Mo.foldM (Df.def :: ITC.CheckState) msgs $ \state msg ->
              let (ret, (_, _, state')) = runST (checkMsg msg) state
              in case ret of
                Left message -> Left message
                Right _ -> Right state'
  in case ret of
    Left message -> Left message
    Right state ->
      if (state ^. ITC.requestMap & Mp.size) > 0
        then Left $ "The following requests went unanswered: " ++ ppShow (state ^. ITC.requestMap)
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
checkMsg :: TrM.TraceMessage -> ST ITC.CheckState (Either Co.ErrorMsg ())
checkMsg msg = do
  case msg of
    TrM.PaxosInsertion paxosId _ paxosLogEntry ->
      case paxosLogEntry of
        PM.Tablet entry -> do
          range <- getT $ ITC.rangeMap . ix paxosId
          case entry of
            PM.Read requestId key timestamp -> do
              payloadM <- getL $ ITC.requestMap . at requestId
              case payloadM of
                Nothing -> return $ entryUnfamiliarE paxosLogEntry
                Just payload ->
                  case payload of
                    CRq.SlaveRead databaseId' tableId' key' timestamp'
                      | (Co.KeySpaceRange databaseId' tableId') == range &&
                        key' == key &&
                        timestamp' == timestamp -> do
                      ITC.tables . ix range .^^* MVS.read key timestamp
                      return $ Right ()
                    _ -> return $ entryIncorrectE paxosLogEntry payload
            PM.Write requestId key value timestamp -> do
              payloadM <- getL $ ITC.requestMap . at requestId
              case payloadM of
                Nothing -> return $ entryUnfamiliarE paxosLogEntry
                Just payload ->
                  case payload of
                    CRq.SlaveWrite databaseId' tableId' key' value' timestamp'
                      | (Co.KeySpaceRange databaseId' tableId') == range &&
                        key' == key &&
                        value' == value &&
                        timestamp' == timestamp -> do
                      ITC.tables . ix range .^^* MVS.write key value requestId timestamp
                      return $ Right ()
                    _ -> return $ entryIncorrectE paxosLogEntry payload
        PM.Slave entry ->
          case entry of
            PM.RangeRead requestId timestamp -> do
              payloadM <- getL $ ITC.requestMap . at requestId
              case payloadM of
                Nothing -> return $ entryUnfamiliarE paxosLogEntry
                Just payload -> do
                  let entryCorrect =
                        case payload of
                        CRq.RangeRead timestamp'
                          | timestamp' == timestamp -> True
                        CRq.SlaveRead _ _ _ timestamp'
                          | timestamp' == timestamp -> True
                        CRq.SlaveWrite _ _ _ _ timestamp'
                          | timestamp' == timestamp -> True
                        _ -> False
                  if entryCorrect
                    then do
                      ITC.keySpaceManager .^^ KSM.read timestamp
                      return $ Right ()
                    else return $ entryIncorrectE paxosLogEntry payload
            PM.RangeWrite requestId timestamp ranges -> do
              payloadM <- getL $ ITC.requestMap . at requestId
              case payloadM of
                Nothing -> return $ entryUnfamiliarE paxosLogEntry
                Just payload -> do
                  case payload of
                    CRq.RangeWrite ranges' timestamp'
                      | ranges' == ranges &&
                        timestamp' == timestamp -> do
                      ITC.keySpaceManager .^^ KSM.write timestamp requestId ranges
                      -- Update the Checkstate with the new ranges
                      Mo.forM ranges $ \range -> do
                        let paxosId = show range
                        hasRange <- ITC.rangeMap .^^^ Mp.member paxosId
                        if hasRange
                          then return ()
                          else do
                            ITC.rangeMap .^^. Mp.insert paxosId range
                            ITC.tables .^^. Mp.insert range Df.def
                            return ()
                      return $ Right ()
                    _ -> return $ entryIncorrectE paxosLogEntry payload
    TrM.ClientRequestReceived request -> do
      ITC.requestMap .^^. at (request ^. CRq.meta . CRq.requestId) ?~ (request ^. CRq.payload)
      return $ Right ()
    TrM.ClientResponseSent response -> do
      let requestId = response ^. CRs.meta . CRs.requestId
      requestPayloadM <- getL $ ITC.requestMap . at requestId
      case requestPayloadM of
        Nothing -> return $ responseNoRequestE response
        Just requestPayload -> do
          ITC.requestMap .^^. Mp.delete requestId
          case response ^. CRs.payload of
            CRs.RangeRead responsePayload -> do
              case requestPayload of
                CRq.RangeRead timestamp -> do
                  -- staticRead ensure the `lat` is beyond `timestamp`
                  res <- ITC.keySpaceManager .^^^ KSM.staticRead timestamp
                  let ranges = case res of
                                 Nothing -> []
                                 Just (_, _, ranges) -> ranges
                  if (CRsRR.Success ranges) == responsePayload
                    then return $ Right ()
                    else return $ responseValueIncorrectE response requestPayload
                _ -> return $ responseTypeIncorrectE response requestPayload
            CRs.RangeWrite responsePayload -> do
              case requestPayload of
                CRq.RangeWrite ranges timestamp -> do
                  -- staticRead ensure the `lat` is beyond `timestamp`
                  res <- ITC.keySpaceManager .^^^ KSM.staticRead timestamp
                  case res of
                    Just (timestamp', requestId', ranges')
                      | requestId' == requestId -> do
                      case responsePayload of
                        CRsRW.Success
                          | ranges' == ranges &&
                            timestamp' == timestamp -> do
                          return $ Right ()
                        _ -> return $ derivedStateIncorrectlyWrittenE requestPayload response
                    Just _
                      | responsePayload == CRsRW.BackwardsWrite -> return $ Right ()
                    _ -> return $ derivedStateIncorrectlyWrittenE requestPayload response
                _ -> return $ responseTypeIncorrectE response requestPayload
            CRs.SlaveRead responsePayload -> do
              case requestPayload of
                CRq.SlaveRead databaseId tableId key timestamp -> do
                  res <- ITC.keySpaceManager .^^^ KSM.staticRead timestamp
                  case res of
                    Just (_, _, ranges')
                      | elem (Co.KeySpaceRange databaseId tableId) ranges' -> do
                      -- staticRead ensure the `lat` is beyond `timestamp`
                      res <- ITC.tables . ix (Co.KeySpaceRange databaseId tableId) .^^^* MVS.staticRead key timestamp
                      let value = case res of
                                    Nothing -> Nothing
                                    Just (value', _, _) -> Just value'
                      if (CRsSR.Success value) == responsePayload
                        then return $ Right ()
                        else return $ responseValueIncorrectE response requestPayload
                    _ | responsePayload == CRsSR.UnknownDB -> return $ Right ()
                    _ -> return $ responseValueIncorrectE response requestPayload
                _ -> return $ responseTypeIncorrectE response requestPayload
            CRs.SlaveWrite responsePayload -> do
              case requestPayload of
                CRq.SlaveWrite databaseId tableId key value timestamp -> do
                  res <- ITC.keySpaceManager .^^^ KSM.staticRead timestamp
                  case res of
                    Just (_, _, ranges')
                      | elem (Co.KeySpaceRange databaseId tableId) ranges' -> do
                      -- staticRead ensure the `lat` is beyond `timestamp`
                      res <- ITC.tables . ix (Co.KeySpaceRange databaseId tableId) .^^^* MVS.staticRead key timestamp
                      case res of
                        Just (value', requestId', timestamp')
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
      return $ Right ()
