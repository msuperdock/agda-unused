{- |
Module: Agda.Unused.Monad.State

A state monad for determining unused code.
-}
module Agda.Unused.Monad.State

  ( -- * Definitions

    ModuleState(..)
  , State

    -- * Interface

  , stateEmpty
  , stateItems
  , stateModules

    -- * Get

  , getModule

    -- * Modify

  , modifyInsert
  , modifyDelete
  , modifyBlock
  , modifyCheck

  ) where

import Agda.Unused.Monad.Reader
  (Environment, askSkip)
import Agda.Unused.Types.Context
  (Context)
import Agda.Unused.Types.Name
  (QName)
import Agda.Unused.Types.Range
  (Range, Range'(..), RangeInfo, rangeContains)
import Agda.Unused.Utils
  (mapDeletes)

import Control.Monad.Reader
  (MonadReader)
import Control.Monad.State
  (MonadState, gets, modify)
import Data.Bool
  (bool)
import Data.Map.Strict
  (Map)
import qualified Data.Map.Strict
  as Map

-- ## Definitions

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
  { stateItems'
    :: !(Map Range RangeInfo)
    -- ^ Ranges for each unused item.
  , stateModules'
    :: !(Map QName ModuleState)
    -- ^ States for each module dependency.
  } deriving Show

-- ## Interface

-- | Construct an empty state.
stateEmpty
  :: State
stateEmpty
  = State mempty mempty

-- | Get a sorted list of state items.
--
-- If one state item contains another (e.g., an @open@ statement containing
-- @using@ directives), then keep only the containing item.
stateItems
  :: State
  -> [(Range, RangeInfo)]
stateItems
  = stateItemsFilter
  . Map.toAscList
  . stateItems'

-- | Get a list of visited modules.
stateModules
  :: State
  -> [QName]
stateModules
  = Map.keys
  . stateModules'

-- Remove nested items.
stateItemsFilter
  :: [(Range, RangeInfo)]
  -> [(Range, RangeInfo)]
stateItemsFilter []
  = []
stateItemsFilter (i : i' : is) | rangeContains (fst i) (fst i')
  = stateItemsFilter (i : is)
stateItemsFilter (i : i' : is) | rangeContains (fst i') (fst i)
  = stateItemsFilter (i' : is)
stateItemsFilter (i : is)
  = i : stateItemsFilter is

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

stateModule
  :: QName
  -> State
  -> Maybe ModuleState
stateModule n (State _ ms)
  = Map.lookup n ms

stateBlock
  :: QName
  -> State
  -> State
stateBlock n (State rs ms)
  = State rs (Map.insert n Blocked ms)

stateCheck
  :: QName
  -> Context
  -> State
  -> State
stateCheck n c (State rs ms)
  = State rs (Map.insert n (Checked c) ms)

-- ## Get

-- | Get the state of a module.
getModule
  :: MonadState State m
  => QName
  -> m (Maybe ModuleState)
getModule n
  = gets (stateModule n)

-- ## Modify

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

-- | Mark that we are beginning to check a module.
modifyBlock
  :: MonadState State m
  => QName
  -> m ()
modifyBlock n
  = modify (stateBlock n)

-- | Record the results of checking a module.
modifyCheck
  :: MonadState State m
  => QName
  -> Context
  -> m ()
modifyCheck n c
  = modify (stateCheck n c)

