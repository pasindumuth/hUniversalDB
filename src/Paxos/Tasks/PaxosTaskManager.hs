{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Paxos.Tasks.PaxosTaskManager (
  PaxosTaskManager,
  handleTask,
  handleRetry,
  handleInsert
) where

import qualified Data.Default as Df
import qualified Data.Maybe as Mb
import qualified Data.Sequence as Sq
import qualified GHC.Generics as Gn
import qualified System.Random as Rn

import qualified Infra.Utils as U
import qualified Paxos.MultiPaxosInstance as MP
import qualified Paxos.PaxosLog as PL
import qualified Paxos.Tasks.Task as Ta
import qualified Proto.Actions.Actions as Ac
import qualified Proto.Common as Co
import qualified Proto.Messages as Ms
import qualified Proto.Messages.PaxosMessages as PM
import Infra.Lens
import Infra.State

data CurrentInsert derivedStateT = CurrentInsert {
  _i'index :: Int,
  _i'entry :: PM.PaxosLogEntry,
  _i'task :: Ta.Task derivedStateT
} deriving (Show)

data PaxosTaskManager derivedStateT = PaxosTaskManager {
  _i'currentInsert :: Maybe (CurrentInsert derivedStateT),
  _i'taskQueue :: Sq.Seq (Ta.Task derivedStateT),
  _i'counter :: Int
} deriving (Gn.Generic, Df.Default, Show)

makeLenses ''CurrentInsert
makeLenses ''PaxosTaskManager

type HandlingState derivedStateT = (
  MP.MultiPaxosInstance,
  derivedStateT,
  PaxosTaskManager derivedStateT,
  Rn.StdGen,
  [Co.EndpointId])

handleTask :: Ta.Task derivedStateT -> ST (HandlingState derivedStateT) ()
handleTask task = do
  derivedState <- getL $ _2
  requestHandled <- lp0 .^ Ta.tryHandling task derivedState
  if requestHandled
    then return ()
    else do
      taskQueue <- _3.i'taskQueue .^^. U.push task
      if Sq.length taskQueue == 1
        then handleNextTask
        else return ()

handleNextTask :: ST (HandlingState derivedStateT) ()
handleNextTask = do
  taskQueue <- getL $ _3.i'taskQueue
  if Sq.length taskQueue > 0
    then do
      let task = U.peek taskQueue
      handleNextTask' task
    else return ()

pollAndNext :: ST (HandlingState derivedStateT) ()
pollAndNext = do
  _3 . i'taskQueue .^^ U.poll
  handleNextTask

handleNextTask' :: Ta.Task derivedStateT -> ST (HandlingState derivedStateT) ()
handleNextTask' task = do
  _3.i'currentInsert .^^. \_ -> Nothing
  derivedState <- getL $ _2
  requestHandled <- lp0 .^ Ta.tryHandling task derivedState
  if requestHandled
    then pollAndNext
    else do
      index <- _1.MP.paxosLog .^^^ PL.nextAvailableIndex
      let entry = Ta.createPLEntry task derivedState
      _3.i'currentInsert .^^. \_ -> Just $ CurrentInsert index entry task
      slaveEIds <- getL $ _5
      lp2 (_1, _4) .^ MP.insertMultiPaxos slaveEIds entry (Ta.msgWrapper task)
      counterValue <- _3.i'counter .^^. (+1)
      addA $ Ac.RetryOutput counterValue

handleRetry :: Int -> ST (HandlingState derivedStateT) ()
handleRetry counterValue = do
  currentInsertM <- getL $ _3.i'currentInsert
  counter <- getL $ _3.i'counter
  case currentInsertM of
    Just (CurrentInsert _ _ task ) | counter == counterValue -> do
      handleNextTask' task
    _ -> return ()

handleInsert :: ST (HandlingState derivedStateT) ()
handleInsert = do
  currentInsertM <- getL $ _3.i'currentInsert
  case currentInsertM of
    Just (CurrentInsert index entry task) -> do
      nextEntryM <- _1.MP.paxosLog .^^^ PL.getPLEntry index
      case nextEntryM of
        Just nextEntry -> do
          if nextEntry == entry
            then do
              derivedState <- getL $ _2
              lp0 .^ Ta.done task derivedState
              _3.i'currentInsert .^^. \_ -> Nothing
              pollAndNext
            else handleNextTask' task
        _ -> return ()
    _ -> return ()
