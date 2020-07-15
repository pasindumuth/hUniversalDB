{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Proto.Actions.MasterActions where

import qualified Data.Binary as Bn
import qualified Data.Default as Df
import qualified GHC.Generics as Gn

import qualified Proto.Actions.Actions as Ac
import qualified Proto.Common as Co
import qualified Proto.Messages as Ms

data InputAction =
  Receive { eId :: Co.EndpointId, msg :: Ms.Message } |
  RetryInput { counterValue :: Int } |
  PerformInput { uid :: Co.UID }
  deriving (Gn.Generic, Bn.Binary, Show)

data OutputAction =
  Send { eIds :: [Co.EndpointId], msg :: Ms.Message} |
  RetryOutput { counterValue :: Int, delay :: Int } |
  Print { message :: String } |
  PerformOutput { uid :: Co.UID, delay :: Int }
  deriving (Gn.Generic, Bn.Binary, Show)

instance Ac.OutputAction OutputAction where
  send = Send
  print = Print
  retry = RetryOutput
