module Slave.Tablet.TabletInputHandler where

import qualified Paxos.MultiPaxosInstance as MP
import qualified Paxos.PaxosLog as PL
import qualified Proto.Actions.Actions as Ac
import qualified Proto.Messages as Ms
import qualified Slave.Tablet.TabletRequestManager as TRM
import qualified Slave.Tablet.DerivedState as DS
import qualified Slave.Tablet.Internal.Env as En
import qualified Slave.Tablet.Internal.GlobalState as GS
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
          TRM.handlingState .^ TRM.handleClientRequest eId request
        Ms.MultiPaxosMessage multiPaxosMsg -> do
          pl <- getL GS.paxosLog
          slaveEIds <- getL $ GS.env.En.slaveEIds
          lp3 (GS.multiPaxosInstance, GS.paxosLog, GS.env.En.rand) .^ MP.handleMultiPaxos eId slaveEIds multiPaxosMsg
          pl' <- getL GS.paxosLog
          if (pl /= pl')
            then do
              addA $ Ac.Print $ show pl'
              GS.derivedState .^ DS.handleDerivedState pl pl'
              TRM.handlingState .^ TRM.handleInsert
            else return ()
    Ac.RetryInput counterValue ->
      TRM.handlingState .^ TRM.handleRetry counterValue
