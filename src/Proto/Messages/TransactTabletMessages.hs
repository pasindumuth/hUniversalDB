{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Proto.Messages.TransactTabletMessages where

import qualified Data.Binary as Bn
import qualified GHC.Generics as Gn

data TransactTabletMessage = TransactTabletMessage
  deriving (Gn.Generic, Bn.Binary, Show)
