{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Transact.Container.Message where

import qualified Data.Binary as Bn
import qualified GHC.Generics as Gn

import qualified Transact.Container.Common as Co
-- Conventions: Forwarded Messages is the best example. Notice the use of
-- ' for the Value Constructors of Types that are supposed to be a simple
-- union of other Types T. The Value Constructor name is T'. We don't
-- need ' when the Value Constructors are complex, like Fw'Metadata.

------------------------------------------------------------------------------------------------------------------------
-- Client Messages
------------------------------------------------------------------------------------------------------------------------
data Cl'Request = Cl'Request deriving (Gn.Generic, Bn.Binary, Show)
data Cl'Response = Cl'Response deriving (Gn.Generic, Bn.Binary, Show)

data Cl'Payload =
  Cl'Request' Cl'Request |
  Cl'Response' Cl'Response
  deriving (Gn.Generic, Bn.Binary, Show)

data Cl'Message = Cl'Message Cl'Payload deriving (Gn.Generic, Bn.Binary, Show)

------------------------------------------------------------------------------------------------------------------------
-- Forwarded Messages
------------------------------------------------------------------------------------------------------------------------
data Fw'Metadata =
  Fw'OriginEndpoint Co.EndpointId |
  Fw'OriginRequest Co.RequestId Cl'Request
  deriving (Gn.Generic, Bn.Binary, Show)

data Fw'Request = Fw'Request deriving (Gn.Generic, Bn.Binary, Show)
data Fw'Response = Fw'Response deriving (Gn.Generic, Bn.Binary, Show)

data Fw'Payload =
  Fw'Request' Fw'Request |
  Fw'Response' Fw'Response
  deriving (Gn.Generic, Bn.Binary, Show)

data Fw'Message = Fw'Message Fw'Metadata Fw'Payload deriving (Gn.Generic, Bn.Binary, Show)

------------------------------------------------------------------------------------------------------------------------
-- Top Level Message
------------------------------------------------------------------------------------------------------------------------
data Message =
  Client' Cl'Message |
  Forwarded' Fw'Message
  deriving (Gn.Generic, Bn.Binary, Show)
