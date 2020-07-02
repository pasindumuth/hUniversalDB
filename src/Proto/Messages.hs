{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Proto.Messages where

import qualified Data.Binary as Bn
import qualified GHC.Generics as Gn

import qualified Proto.Common as Co
import qualified Proto.Messages.ClientMessages as CM
import qualified Proto.Messages.PaxosMessages as PM
import qualified Proto.Messages.SlaveMessages as SM
import qualified Proto.Messages.TabletMessages as TM

-- TODO maybe we can use prisms to construct Message from one of the constituents
data Message =
  ClientRequest CM.ClientRequest |
  ClientResponse CM.ClientResponse |
  SlaveMessage SM.SlaveMessage |
  TabletMessage Co.KeySpaceRange TM.TabletMessage
  deriving (Gn.Generic, Bn.Binary, Show)
