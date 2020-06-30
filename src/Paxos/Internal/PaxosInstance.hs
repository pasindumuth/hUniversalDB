{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Paxos.Internal.PaxosInstance where

import qualified Data.Default as Df
import qualified Data.Map.Strict as Mp
import qualified GHC.Generics as Gn

import qualified Proto.Messages.PaxosMessages as M
import Infra.Lens

data Proposal = Proposal {
  _crnd :: M.Rnd,
  _cval :: M.Val,
  _promises :: [(M.Rnd, Maybe M.Val)]
} deriving (Show)

data ProposerState = ProposerState {
  _proposals :: Mp.Map M.Rnd Proposal
} deriving (Gn.Generic, Df.Default, Show)

data AcceptorState = AcceptorState {
  _rnd :: M.Rnd,
  _vrnd :: M.Rnd,
  _vval :: Maybe M.Val
} deriving (Gn.Generic, Df.Default, Show)

data LearnerState = LearnerState {
  _learns :: Mp.Map M.Rnd (M.Val, Int)
} deriving (Gn.Generic, Df.Default, Show)

data PaxosInstance = PaxosInstance {
  _proposerState :: ProposerState,
  _acceptorState :: AcceptorState,
  _learnerState :: LearnerState
} deriving (Gn.Generic, Df.Default, Show)

makeLenses ''Proposal
makeLenses ''ProposerState
makeLenses ''AcceptorState
makeLenses ''LearnerState
makeLenses ''PaxosInstance
