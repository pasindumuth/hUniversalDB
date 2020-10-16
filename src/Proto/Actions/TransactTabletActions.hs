{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Proto.Actions.TransactTabletActions where

import qualified Data.Binary as Bn
import qualified Data.Default as Df
import qualified GHC.Generics as Gn

import qualified Proto.Actions.Actions as Ac
import qualified Proto.Common as Co
import qualified Proto.Messages as Ms
import qualified Proto.Messages.TransactTabletMessages as TTM

data InputAction =
  Receive { eId :: Co.EndpointId, tabletMsg :: TTM.TransactTabletMessage } |
  RetryInput { counterValue :: Int }
  deriving (Gn.Generic, Bn.Binary, Show)

data OutputAction =
  Send { eIds :: [Co.EndpointId], msg :: Ms.Message} |
  RetryOutput { counterValue :: Int, delay :: Int } |
  Print { message :: String }
  deriving (Gn.Generic, Bn.Binary, Show)

instance Ac.OutputAction OutputAction where
  send = Send
  print = Print
  retry = RetryOutput
