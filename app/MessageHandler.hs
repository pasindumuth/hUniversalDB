{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module MessageHandler where

import qualified Message as M

handleMessage :: M.Message -> M.MultiPaxosMessage
handleMessage msg =
  case msg of
    M.MMessage mpMsg -> mpMsg
    M.ClientMessage val -> M.Insert $ M.Write val
  