{- |
Module: Agda.Unused.Monad.State

A state monad for determining unused code.
-}
module Agda.Unused.Monad.State

  ( -- * Definitions

    ModuleState(..)
  , State(..)

    -- * Interface

  , stateEmpty
  , stateBlock
  , stateCheck
  , stateLookup

    -- * Modify

  , modifyDelete
  , modifyInsert

  ) where

import Agda.Unused.Monad.Reader
  (Environment, askSkip)
import Agda.Unused.Types.Context
  (Context)
import Agda.Unused.Types.Name
  (QName)
import Agda.Unused.Types.Range
  (Range, Range'(..), RangeInfo)
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

-- | Cache the results of checking modules. This allows us to:
-- 
-- - Avoid duplicate computations.
-- - Handle cyclic module dependencies without nontermination.
data ModuleState where

  Blocked
    :: ModuleState

  Checked
    :: !Context
    -> ModuleState

  deriving Show

-- | The current computation state.
data State
  = State
  { stateUnused
    :: !(Map Range RangeInfo)
    -- ^ Ranges for each unused item.
  , stateModules
    :: !(Map QName ModuleState)
    -- ^ State for module dependencies.
  } deriving Show

-- | Construct an empty state.
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

-- | Lookup the state of a module.
stateLookup
  :: QName
  -> State
  -> Maybe ModuleState
stateLookup m (State _ ms)
  = Map.lookup m ms

-- | Mark that we are beginning to check a module.
stateBlock
  :: QName
  -> State
  -> State
stateBlock m (State rs ms)
  = State rs (Map.insert m Blocked ms)

-- | Record the results of checking a module.
stateCheck
  :: QName
  -> Context
  -> State
  -> State
stateCheck m c (State rs ms)
  = State rs (Map.insert m (Checked c) ms)

-- | Record a new unused item.
modifyInsert
  :: MonadReader Environment m
  => MonadState State m
  => Range
  -> RangeInfo
  -> m ()
modifyInsert r i
  = askSkip >>= bool (modify (stateInsert r i)) (pure ())

-- | Mark a list of items as used.
modifyDelete
  :: MonadReader Environment m
  => MonadState State m
  => [Range]
  -> m ()
modifyDelete rs
  = askSkip >>= bool (modify (stateDelete rs)) (pure ())

