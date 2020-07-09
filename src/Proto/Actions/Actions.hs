{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Proto.Actions.Actions where

import qualified Data.Binary as Bn
import qualified Data.Default as Df
import qualified GHC.Generics as Gn

import qualified Proto.Common as Co
import qualified Proto.Messages as Ms
import qualified Proto.Messages.TabletMessages as TM

data InputAction =
  Receive { eId :: Co.EndpointId, msg :: Ms.Message } |
  RetryInput { counterValue :: Int }
  deriving (Gn.Generic, Bn.Binary, Show)

-- TODO: should we create TabletActions.hs? If so, do we also make TabletOutputAction?
data TabletInputAction =
  TabletReceive { eId :: Co.EndpointId, tabletMsg :: TM.TabletMessage } |
  TabletRetryInput { counterValue :: Int }
  deriving (Gn.Generic, Bn.Binary, Show)

data OutputAction =
  Send { eIds :: [Co.EndpointId], msg :: Ms.Message} |
  -- TODO: the common code should have control over how long it should wait
  -- until the next repeat We can also generalize RetryOutput to contain
  -- generic data, and RetryInput also contains generic data.
  RetryOutput { counterValue :: Int } |
  Print { message :: String } |
  Slave_CreateTablet { ranges :: [Co.KeySpaceRange] } |
  TabletForward { range :: Co.KeySpaceRange, eId :: Co.EndpointId, tabletMsg :: TM.TabletMessage }
  deriving (Gn.Generic, Bn.Binary, Show)
