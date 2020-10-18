{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Transact.Container.Actions where

import qualified Data.Binary as Bn
import qualified Data.Default as Df
import qualified GHC.Generics as Gn

import qualified Transact.Container.Common as Co
import qualified Transact.Container.Message as Ms

--------------------------------------------------------------------------
-- Server Actions
--------------------------------------------------------------------------
data S'InputAction =
  S'Receive { eId :: Co.EndpointId, msg :: Ms.Message }
  deriving (Gn.Generic, Bn.Binary, Show)

data S'OutputAction =
  S'Send { eIds :: [Co.EndpointId], msg :: Ms.Message} |
  S'Print { message :: String } |
  S'TabletForward { tabletId :: Co.TabletShape, eId :: Co.EndpointId, msg :: Ms.Message }
  deriving (Gn.Generic, Bn.Binary, Show)

--------------------------------------------------------------------------
-- Tablet Actions
--------------------------------------------------------------------------
data T'InputAction =
  T'Receive { eId :: Co.EndpointId, msg :: Ms.Message }
  deriving (Gn.Generic, Bn.Binary, Show)

data T'OutputAction =
  T'Send { eIds :: [Co.EndpointId], msg :: Ms.Message} |
  T'Print { message :: String }
  deriving (Gn.Generic, Bn.Binary, Show)
