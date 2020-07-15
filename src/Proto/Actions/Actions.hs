{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Proto.Actions.Actions where

import qualified Proto.Common as Co
import qualified Proto.Messages as Ms

class OutputAction a where
  send :: [Co.EndpointId] -> Ms.Message -> a
  print :: String -> a
  retry :: Int -> a
