{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Logging where

import qualified System.Log.Logger as L

-- re-exported logging functions
infoM = L.infoM
debugM = L.debugM

-- The names of the various loggers we have
paxos = "paxos"
network = "network"
main = "main"

setPriority :: String -> L.Priority -> IO ()
setPriority name p = do
  logger <- L.getLogger name
  L.saveGlobalLogger $ L.setLevel p logger

setupLogging :: IO ()
setupLogging = do
  setPriority paxos L.INFO
  setPriority network L.INFO
  setPriority main L.INFO
