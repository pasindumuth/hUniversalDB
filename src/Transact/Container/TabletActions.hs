{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Transact.Container.TabletActions where

import qualified Data.Binary as Bn
import qualified Data.Default as Df
import qualified GHC.Generics as Gn

import qualified Transact.Container.Common as Co
import qualified Transact.Container.Message as Ms

data InputAction =
  Receive { eId :: Co.EndpointId, msg :: Ms.Message }
  deriving (Gn.Generic, Bn.Binary, Show)

data OutputAction =
  Send { eIds :: [Co.EndpointId], msg :: Ms.Message} |
  Print { message :: String }
  deriving (Gn.Generic, Bn.Binary, Show)
