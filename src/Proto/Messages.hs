{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Proto.Messages where

import qualified Data.Binary as Bn
import qualified GHC.Generics as Gn

import qualified Proto.Common as Co
import qualified Proto.Messages.ClientRequests as CRq
import qualified Proto.Messages.ClientResponses as CRs
import qualified Proto.Messages.MasterMessages as MM
import qualified Proto.Messages.PaxosMessages as PM
import qualified Proto.Messages.SlaveMessages as SM
import qualified Proto.Messages.TabletMessages as TM
import qualified Proto.Messages.TransactTabletMessages as TTM

data Message =
  ClientRequest CRq.ClientRequest |
  ClientResponse CRs.ClientResponse |
  SlaveMessage SM.SlaveMessage |
  MasterMessage MM.MasterMessage |
  TabletMessage Co.TabletId TM.TabletMessage |
  TransactTabletMessage Co.TabletId TTM.TransactTabletMessage
  deriving (Gn.Generic, Bn.Binary, Show)
