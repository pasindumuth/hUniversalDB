module InputActionHandler where

import qualified ClientRequestManager as CRM
import qualified MultiPaxosInstance as MP
import qualified PaxosLog as PL
import qualified Records.GlobalState as GS
import qualified Records.Actions.Actions as A
import qualified Records.Messages.Messages as M
import Lens (lp3)
import State (ST, (.^), (.^^^))

handleInputAction
  :: A.InputAction
  -> ST GS.GlobalState ()
handleInputAction iAction =
  case iAction of
    A.Receive eId msg ->
      case msg of
        M.ClientRequest request ->
          CRM.handlingState .^ CRM.handleClientRequest eId request
        M.MultiPaxosMessage multiPaxosMsg -> do
          index <- GS.paxosLog .^^^ PL.nextAvailableIndex
          (lp3 (GS.multiPaxosInstance, GS.paxosLog, GS.env)) .^ MP.handleMultiPaxos eId multiPaxosMsg
          index' <- GS.paxosLog .^^^ PL.nextAvailableIndex
          if (index /= index')
            then CRM.handlingState .^ CRM.handleInsert
            else return ()
    A.RetryInput clockValue ->
      CRM.handlingState .^ CRM.handleRetry clockValue
