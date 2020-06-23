{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module MessageHandler where

import qualified Records.Messages.ClientMessages as CM
import qualified Records.Messages.Messages as M
import qualified Records.Messages.PaxosMessages as PM

handleMessage :: M.Message -> PM.MultiPaxosMessage
handleMessage msg =
  case msg of
    M.MultiPaxosMessage mpMsg -> mpMsg
    M.ClientRequest request ->
      case request of
        CM.ReadRequest key timestamp -> PM.Insert $ PM.Read key timestamp
        CM.WriteRequest key value timestamp -> PM.Insert $ PM.Write key value timestamp
