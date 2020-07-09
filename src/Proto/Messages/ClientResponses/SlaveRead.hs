{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Proto.Messages.ClientResponses.SlaveRead where

import qualified Data.Binary as Bn
import qualified GHC.Generics as Gn

data SlaveRead =
  UnknownDB |
  Success { value :: Maybe String }
  deriving (Gn.Generic, Bn.Binary, Show)
