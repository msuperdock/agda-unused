module Agda.Unused.Monad.State
  ( ModuleState (..)
  , State (..)
  , modifyDelete
  , modifyInsert
  , stateCheck
  , stateBlock
  , stateEmpty
  , stateLookup
  ) where

import Agda.Unused.Monad.Context
  (Context)
import Agda.Unused.Monad.Reader
  (Environment, askBuiltin)
import Agda.Unused.Types.Name
  (QName)
import Agda.Unused.Types.Range
  (Range, Range' (..), RangeInfo)
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
stateInsert NoRange _ s
  = s
stateInsert r@(Range _ _) i (State rs ms)
  = State (Map.insert r i rs) ms

stateDelete
  :: [Range]
  -> State
  -> State
stateDelete rs (State rs' ms)
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

modifyInsert
  :: MonadReader Environment m
  => MonadState State m
  => Range
  -> RangeInfo
  -> m ()
modifyInsert r i
  = askBuiltin
  >>= bool (modify (stateInsert r i)) (pure ())

modifyDelete
  :: MonadReader Environment m
  => MonadState State m
  => [Range]
  -> m ()
modifyDelete rs
  = askBuiltin
  >>= bool (modify (stateDelete rs)) (pure ())

