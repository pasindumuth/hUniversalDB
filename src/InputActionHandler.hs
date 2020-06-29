module InputActionHandler where

import qualified ClientRequestManager as CRM
import qualified Paxos.MultiPaxosInstance as MP
import qualified DerivedState as DS
import qualified Paxos.PaxosLog as PL
import qualified Records.GlobalState as GS
import qualified Proto.Actions.Actions as Ac
import qualified Proto.Messages as Ms
import Lens
import State

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
          lp3 (GS.multiPaxosInstance, GS.paxosLog, GS.env) .^ MP.handleMultiPaxos eId multiPaxosMsg
          pl' <- getL GS.paxosLog
          if (pl /= pl')
            then do
              addA $ Ac.Print $ show pl'
              GS.derivedState .^ DS.handleDerivedState pl pl'
              CRM.handlingState .^ CRM.handleInsert
            else return ()
    Ac.RetryInput counterValue ->
      CRM.handlingState .^ CRM.handleRetry counterValue
