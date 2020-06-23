module ClientRequestManager where

import qualified PaxosLog as PL
import qualified DerivedState as DS
import qualified Records.ClientRequestManager as CRM
import qualified Records.Common.Common as C
import qualified Records.Messages.ClientMessages as CM
import qualified Records.Actions.Actions as A

handleClientRequest
  :: C.EndpointId
  -> CM.ClientRequest
  -> CRM.ClientRequestManager
  -> ([A.OutputAction], CRM.ClientRequestManager)
handleClientRequest eId request crm = ([], crm)

handleNextRequest
  :: CRM.ClientRequestManager
  -> ([A.OutputAction], CRM.ClientRequestManager)
handleNextRequest crm = ([], crm)

handleRetry
  :: Int
  -> CRM.ClientRequestManager
  -> ([A.OutputAction], CRM.ClientRequestManager)
handleRetry try crm = ([], crm)

handleInsert
  :: PL.PaxosLog
  -> DS.DerivedState
  -> CRM.ClientRequestManager
  -> ([A.OutputAction], CRM.ClientRequestManager)
handleInsert pl state crm = ([], crm)
