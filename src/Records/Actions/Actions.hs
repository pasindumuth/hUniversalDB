{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Records.Actions.Actions where

import qualified Data.Binary as Bn
import qualified Data.Default as Df
import qualified GHC.Generics as Gn

import qualified Records.Common.Common as Co
import qualified Records.Messages.Messages as Ms

data InputAction =
  Receive { eId :: Co.EndpointId, msg :: Ms.Message } |
  RetryInput { counterValue :: Int }
  deriving (Gn.Generic, Bn.Binary, Show)

data OutputAction =
  Send { eIds :: [Co.EndpointId], msg :: Ms.Message} |
  RetryOutput { counterValue :: Int } |
  Print { message :: String }
