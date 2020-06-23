{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module MessageHandler where

import qualified Message as M

handleMessage :: M.Message -> M.MultiPaxosMessage
handleMessage msg =
  case msg of
    M.MultiPaxosMessage mpMsg -> mpMsg
    M.ClientMessage key -> M.Insert $ M.Write key "value" 1
  