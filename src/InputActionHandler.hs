module InputActionHandler where

import qualified ClientRequestManager as CRM
import qualified MultiPaxosInstance as MP
import qualified Records.GlobalState as GS
import qualified Records.Actions.Actions as A
import qualified Records.Messages.Messages as M
import Lens ((%~), (.~), (^.), (&), (?~), at, ix, (.^.), wrapMaybe)

handleInputAction
  :: A.InputAction
  -> GS.GlobalState
  -> ([A.OutputAction], GS.GlobalState)
handleInputAction iAction g =
  case iAction of
    A.Receive eId msg ->
      case msg of
        M.ClientRequest request -> g .^. GS.clientRequestManager $ CRM.handleClientRequest eId request
