{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Records.PaxosInstance where

import qualified Data.Default as D
import qualified Data.Map.Strict as Mp
import qualified GHC.Generics as G

import qualified Records.Messages.PaxosMessages as M
import Lens

data Proposal = Proposal {
  _crnd :: M.Rnd,
  _cval :: M.Val,
  _promises :: [(M.Rnd, M.Val)]
} deriving (Show)

data ProposerState = ProposerState {
  _proposals :: Mp.Map M.Rnd Proposal
} deriving (G.Generic, D.Default, Show)

data AcceptorState = AcceptorState {
  _rnd :: M.Rnd,
  _vrnd :: M.Rnd,
  _vval :: M.Val
} deriving (G.Generic, D.Default, Show)

data LearnerState = LearnerState {
  _learns :: Mp.Map M.Rnd (M.Val, Int)
} deriving (G.Generic, D.Default, Show)

data PaxosInstance = PaxosInstance {
  _proposerState :: ProposerState,
  _acceptorState :: AcceptorState,
  _learnerState :: LearnerState
} deriving (G.Generic, D.Default, Show)

makeLenses ''Proposal
makeLenses ''ProposerState
makeLenses ''AcceptorState
makeLenses ''LearnerState
makeLenses ''PaxosInstance
