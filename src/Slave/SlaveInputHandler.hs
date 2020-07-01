module Slave.SlaveInputHandler where

import qualified System.Random as Rn

import qualified Paxos.MultiPaxosInstance as MP
import qualified Paxos.PaxosLog as PL
import qualified Paxos.Tasks.PaxosTaskManager as PTM
import qualified Paxos.Tasks.Task as Ta
import qualified Proto.Actions.Actions as Ac
import qualified Proto.Common as Co
import qualified Proto.Messages as Ms
import qualified Proto.Messages.ClientMessages as CM
import qualified Proto.Messages.PaxosMessages as PM
import qualified Slave.DerivedState as DS
import qualified Slave.Env as En
import qualified Slave.SlaveState as SS
import qualified Slave.Internal_DerivedState as IDS
import qualified Slave.Internal_KeySpaceManager as IKSM
import Infra.Lens
import Infra.State

handlingState :: Lens' SS.SlaveState (
  PL.PaxosLog,
  MP.MultiPaxosInstance,
  DS.DerivedState,
  PTM.PaxosTaskManager DS.DerivedState,
  Rn.StdGen,
  [Co.EndpointId])
handlingState =
  (lp6 (
    SS.paxosLog,
    SS.multiPaxosInstance,
    SS.derivedState,
    SS.paxosTaskManager,
    SS.env.En.rand,
    SS.env.En.slaveEIds))

handleInputAction
  :: Ac.InputAction
  -> ST SS.SlaveState ()
handleInputAction iAction =
  case iAction of
    Ac.Receive eId msg ->
      case msg of
        Ms.ClientRequest request ->
          case request of
            CM.CreateDatabase _ _ -> handlingState .^ (PTM.handleTask $ createClientTask eId request)
        Ms.MultiPaxosMessage multiPaxosMsg -> do
          pl <- getL SS.paxosLog
          slaveEIds <- getL $ SS.env.En.slaveEIds
          lp3 (SS.multiPaxosInstance, SS.paxosLog, SS.env.En.rand) .^ MP.handleMultiPaxos eId slaveEIds multiPaxosMsg
          pl' <- getL SS.paxosLog
          if (pl /= pl')
            then do
              SS.derivedState .^ DS.handleDerivedState pl pl'
              handlingState .^ PTM.handleInsert
            else return ()

createClientTask :: Co.EndpointId -> CM.ClientRequest -> Ta.Task DS.DerivedState
createClientTask eId request =
  let description = show (eId, request)
  in case request of
    CM.CreateDatabase databaseId tableId ->
      let description = description
          tryHandling _ = do return False
          done _ = do return ()
          createPLEntry derivedState =
            let range = Co.KeySpaceRange databaseId tableId Nothing Nothing
                generation = (derivedState ^. IDS.keySpaceManager.IKSM.generation) + 1
            in PM.Slave_AddRange range generation
      in Ta.Task description tryHandling done createPLEntry
