module Paxos.Tasks.PaxosTaskManager (
  PTM.PaxosTaskManager,
  handleTask,
  handleRetry,
  handleInsert
) where

import qualified Data.Maybe as Mb
import qualified Data.Sequence as Sq
import qualified System.Random as Rn

import qualified Infra.Utils as U
import qualified Paxos.MultiPaxosInstance as MP
import qualified Paxos.PaxosLog as PL
import qualified Paxos.Tasks.Task as Ta
import qualified Paxos.Tasks.Internal_PaxosTaskManager as PTM
import qualified Proto.Actions.Actions as Ac
import qualified Proto.Common as Co
import qualified Proto.Messages.PaxosMessages as PM
import Infra.Lens
import Infra.State

type HandlingState a = (PL.PaxosLog, MP.MultiPaxosInstance, a, PTM.PaxosTaskManager a, Rn.StdGen, [Co.EndpointId])

handleTask :: Ta.Task a -> ST (HandlingState a) ()
handleTask task = do
  derivedState <- getL $ _3
  requestHandled <- lp0 .^ Ta.tryHandling task derivedState
  if requestHandled
    then return ()
    else do
      taskQueue <- _4.PTM.taskQueue .^^. U.push task
      if Sq.length taskQueue == 1
        then handleNextTask
        else return ()

handleNextTask :: ST (HandlingState a) ()
handleNextTask = do
  taskQueue <- getL $ _4.PTM.taskQueue
  if Sq.length taskQueue > 0
    then do
      let task = U.peek taskQueue
      handleNextTask' task
    else return ()

pollAndNext :: ST (HandlingState a) ()
pollAndNext = do
  _4 . PTM.taskQueue .^^ U.poll
  handleNextTask

handleNextTask' :: Ta.Task a -> ST (HandlingState a) ()
handleNextTask' task = do
  _4.PTM.currentInsert .^^. \_ -> Nothing
  derivedState <- getL $ _3
  requestHandled <- lp0 .^ Ta.tryHandling task derivedState
  if requestHandled
    then pollAndNext
    else do
      index <- _1 .^^^ PL.nextAvailableIndex
      let entry = Ta.createPLEntry task derivedState
      _4.PTM.currentInsert .^^. \_ -> Just $ PTM.CurrentInsert index entry task
      slaveEIds <- getL $ _6
      lp3 (_2, _1, _5) .^ MP.insertMultiPaxos slaveEIds entry
      counterValue <- _4.PTM.counter .^^. (+1)
      addA $ Ac.RetryOutput counterValue

handleRetry :: Int -> ST (HandlingState a) ()
handleRetry counterValue = do
  currentInsertM <- getL $ _4.PTM.currentInsert
  counter <- getL $ _4.PTM.counter
  case currentInsertM of
    Just (PTM.CurrentInsert _ _ task ) | counter == counterValue -> do
      handleNextTask' task
    _ -> return ()

handleInsert :: ST (HandlingState a) ()
handleInsert = do
  currentInsertM <- getL $ _4.PTM.currentInsert
  case currentInsertM of
    Just (PTM.CurrentInsert index entry task) -> do
      nextEntryM <- _1 .^^^ PL.getPLEntry index
      case nextEntryM of
        Just nextEntry -> do
          if nextEntry == entry
            then do
              derivedState <- getL $ _3
              lp0 .^ Ta.done task derivedState
              _4.PTM.currentInsert .^^. \_ -> Nothing
              pollAndNext
            else handleNextTask' task
        _ -> return ()
    _ -> return ()
