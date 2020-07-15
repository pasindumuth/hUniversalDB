{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Proto.Actions.SlaveActions where

import qualified Data.Binary as Bn
import qualified Data.Default as Df
import qualified GHC.Generics as Gn

import qualified Proto.Actions.Actions as Ac
import qualified Proto.Common as Co
import qualified Proto.Messages as Ms
import qualified Proto.Messages.TabletMessages as TM

data InputAction =
  Receive { eId :: Co.EndpointId, msg :: Ms.Message } |
  RetryInput { counterValue :: Int }
  deriving (Gn.Generic, Bn.Binary, Show)

data OutputAction =
  Send { eIds :: [Co.EndpointId], msg :: Ms.Message} |
  RetryOutput { counterValue :: Int } |
  Print { message :: String } |
  Slave_CreateTablet { ranges :: [Co.KeySpaceRange] } |
  TabletForward { range :: Co.KeySpaceRange, eId :: Co.EndpointId, tabletMsg :: TM.TabletMessage }
  deriving (Gn.Generic, Bn.Binary, Show)

instance Ac.OutputAction OutputAction where
  send = Send
  print = Print
  retry = RetryOutput
