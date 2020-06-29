module Slave.InputActionHandler where

import qualified Paxos.MultiPaxosInstance as MP
import qualified Paxos.PaxosLog as PL
import qualified Proto.Actions.Actions as Ac
import qualified Proto.Messages as Ms
import qualified Slave.ClientRequestManager as CRM
import qualified Slave.DerivedState as DS
import qualified Slave.Internal.Env as En
import qualified Slave.Internal.GlobalState as GS
import Infra.Lens
import Infra.State

handleInputAction
  :: Ac.InputAction
  -> ST GS.GlobalState ()
handleInputAction iAction =
  case iAction of
    Ac.Receive eId msg ->
      case msg of
        Ms.ClientRequest request ->
          CRM.handlingState .^ CRM.handleClientRequest eId request
        Ms.MultiPaxosMessage multiPaxosMsg -> do
          pl <- getL GS.paxosLog
          slaveEIds <- getL $ GS.env.En.slaveEIds
          lp3 (GS.multiPaxosInstance, GS.paxosLog, GS.env.En.rand) .^ MP.handleMultiPaxos eId slaveEIds multiPaxosMsg
          pl' <- getL GS.paxosLog
          if (pl /= pl')
            then do
              addA $ Ac.Print $ show pl'
              GS.derivedState .^ DS.handleDerivedState pl pl'
              CRM.handlingState .^ CRM.handleInsert
            else return ()
    Ac.RetryInput counterValue ->
      CRM.handlingState .^ CRM.handleRetry counterValue
