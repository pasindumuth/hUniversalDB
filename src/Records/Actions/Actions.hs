{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Records.Actions.Actions where

import qualified Data.Binary as B
import qualified Data.Default as D
import qualified GHC.Generics as G

import qualified Records.Common.Common as C
import qualified Records.Messages.Messages as M

data InputAction =
  Receive { eId :: C.EndpointId, msg :: M.Message } |
  RetryInput { counterValue :: Int }
  deriving (G.Generic, B.Binary, Show)

data OutputAction =
  Send { eIds :: [C.EndpointId], msg :: M.Message} |
  RetryOutput { counterValue :: Int } |
  Print { message :: String }
