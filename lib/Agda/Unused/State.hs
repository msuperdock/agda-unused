module Agda.Unused.State
  ( ModuleState (..)
  , State (..)
  , modifyDeletes
  , stateCheck
  , stateBlock
  , stateEmpty
  , stateInsert
  , stateLookup
  ) where

import Agda.Unused.Context
  (Context)
import Agda.Unused.Environment
  (Environment, askBuiltin)
import Agda.Unused.Name
  (QName)
import Agda.Unused.Range
  (Range, RangeInfo)
import Agda.Unused.Utils
  (mapDeletes)

import Control.Monad.Reader
  (MonadReader)
import Control.Monad.State
  (MonadState, modify)
import Data.Bool
  (bool)
import Data.Map.Strict
  (Map)
import qualified Data.Map.Strict
  as Map

data ModuleState where

  Blocked
    :: ModuleState

  Checked
    :: !Context
    -> ModuleState

  deriving Show

data State
  = State
  { stateUnused
    :: !(Map Range RangeInfo)
  , stateModules
    :: !(Map QName ModuleState)
  } deriving Show

stateEmpty
  :: State
stateEmpty
  = State mempty mempty

stateInsert
  :: Range
  -> RangeInfo
  -> State
  -> State
stateInsert r i (State rs ms)
  = State (Map.insert r i rs) ms

stateDeletes
  :: [Range]
  -> State
  -> State
stateDeletes rs (State rs' ms)
  = State (mapDeletes rs rs') ms

stateLookup
  :: QName
  -> State
  -> Maybe ModuleState
stateLookup m (State _ ms)
  = Map.lookup m ms

stateBlock
  :: QName
  -> State
  -> State
stateBlock m (State rs ms)
  = State rs (Map.insert m Blocked ms)

stateCheck
  :: QName
  -> Context
  -> State
  -> State
stateCheck m c (State rs ms)
  = State rs (Map.insert m (Checked c) ms)

modifyDeletes
  :: MonadReader Environment m
  => MonadState State m
  => [Range]
  -> m ()
modifyDeletes rs
  = askBuiltin >>= bool (modify (stateDeletes rs)) (pure ())

