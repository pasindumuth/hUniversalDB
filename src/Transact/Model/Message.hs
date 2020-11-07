{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Transact.Model.Message where

import qualified Data.Binary as Bn
import qualified GHC.Generics as Gn

import qualified Common.Model.RelationalTablet as RT
import qualified Transact.Model.Common as Co

-- Conventions: Forwarded Messages is the best example. The main rule here
-- is that if we have a Type Constructor like T T in a type S, then we should
-- change it to T' T. This is because generally, Type Constructor T might
-- be used as in the definition of Type T. For consistency, we always leave
-- the Type Constructor T for this purpose. The second, more obvious rules
-- is that we should qualify every symbol with it's namespace.

------------------------------------------------------------------------------------------------------------------------
-- Client Messages
------------------------------------------------------------------------------------------------------------------------
-- These are the messages that are sent by the client.

data Cl'Request = Cl'Request deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)
data Cl'Response = Cl'Response deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)

data Cl'Payload
  = Cl'Request' Cl'Request
  | Cl'Response' Cl'Response
  deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)

data Cl'Message = Cl'Message Cl'Payload deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)

------------------------------------------------------------------------------------------------------------------------
-- Forwarded Messages
------------------------------------------------------------------------------------------------------------------------
-- These are client messages that were forwarded from the node
-- that received the message to some other node that can actually
-- handle the client message.

data Fw'Metadata
  = Fw'OriginEndpoint Co.EndpointId
  | Fw'OriginRequest Co.RequestId Cl'Request
  deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)

data Fw'Request = Fw'Request deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)
data Fw'Response = Fw'Response deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)

data Fw'Payload
  = Fw'Request' Fw'Request
  | Fw'Response' Fw'Response
  deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)

data Fw'Message = Fw'Message Fw'Metadata Fw'Payload deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)

------------------------------------------------------------------------------------------------------------------------
-- Admin Messages
------------------------------------------------------------------------------------------------------------------------
-- These are messages sent by the administrators of the system.
-- This is a back door for forcefully modifying the system. These
-- messages give the power for admins to add/delete data non-transactionally,
-- among other things. Integration tests can conveniently use these messages
-- to setup the system to a desirable state.

data Ad'Request
  -- | Contains the row & timestamp to insert.
  = Ad'InsertRq Co.TabletPath RT.Row Co.Timestamp
  -- | Contains the primaryKey, column to update and value & timestamp to update it to.
  | Ad'UpdateRq Co.TabletPath RT.PrimaryKey String (Maybe RT.ColumnValue)  Co.Timestamp
  -- | Contains the primaryKey of the row to delete and the timestamp to delete at.
  | Ad'DeleteRq Co.TabletPath RT.PrimaryKey Co.Timestamp
  -- | Contains the primaryKey of the row to read and the timestamp to read at.
  | Ad'ReadRowRq Co.TabletPath RT.PrimaryKey Co.Timestamp
  deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)

data Ad'Response
  = Ad'ReadRowRs (Maybe RT.Row) Co.Timestamp
  deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)

data Ad'Metadata = Ad'Metadata Co.RequestId deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)

data Ad'Payload
  = Ad'Request' Ad'Request
  | Ad'Response' Ad'Response
  deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)

data Ad'Message = Ad'Message Ad'Metadata Ad'Payload deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)

------------------------------------------------------------------------------------------------------------------------
-- Top Level Message
------------------------------------------------------------------------------------------------------------------------
data Message
  = Client Cl'Message
  | Forwarded Fw'Message
  | Admin Ad'Message
  deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)

------------------------------------------------------------------------------------------------------------------------
-- Trace Message
------------------------------------------------------------------------------------------------------------------------
-- These are used for tracing the program for testing purposes.

data Tr'TraceMessage
  = Tr'AdminResponse Ad'Response
  deriving (Gn.Generic, Bn.Binary, Show, Eq, Ord)
