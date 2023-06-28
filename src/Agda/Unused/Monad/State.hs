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

  , getHash
  , getModule
  , getSources

    -- * Modify

  , modifyInsert
  , modifyDelete
  , modifyBlock
  , modifyCheck
  , modifySources

  ) where

import Agda.Unused.Monad.Reader
  (Environment, askSkip)
import Agda.Unused.Types.Context
  (Context)
import Agda.Unused.Types.Name
  (QName)
import Agda.Unused.Types.Range
  (RangeInfo, rangeContains)

import Agda.Syntax.Common
  (ModuleNameHash(..))
import Agda.Syntax.Position
  (Range, Range'(..))
import Agda.TypeChecking.Monad.Base
  (ModuleToSource)
import Control.Monad
  (unless)
import Control.Monad.Reader
  (MonadReader)
import Control.Monad.State
  (MonadState, gets, modify)
import Data.Map.Strict
  (Map)
import qualified Data.Map.Strict
  as Map
import Data.Set
  (Set)
import Data.Word
  (Word64)

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
  , stateSources'
    :: !ModuleToSource
    -- ^ A cache of source paths corresponding to certain module names.
  , stateHash
    :: !Word64
    -- ^ An integer to use as the next module hash.
  } deriving Show

-- ## Interface

-- | Construct an empty state.
stateEmpty
  :: State
stateEmpty
  = State mempty mempty mempty 0

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

-- | Get a list of visited modules.
stateModules
  :: State
  -> Set QName
stateModules
  = Map.keysSet
  . stateModules'

stateSources
  :: ModuleToSource
  -> State
  -> State
stateSources ss (State rs ms _ h)
  = State rs ms ss h

stateInsert
  :: Range
  -> RangeInfo
  -> State
  -> State
stateInsert NoRange _ s
  = s
stateInsert r@(Range _ _) i (State rs ms ss h)
  = State (Map.insert r i rs) ms ss h

stateDelete
  :: Set Range
  -> State
  -> State
stateDelete rs (State rs' ms ss h)
  = State (Map.withoutKeys rs' rs) ms ss h

stateModule
  :: QName
  -> State
  -> Maybe ModuleState
stateModule n (State _ ms _ _)
  = Map.lookup n ms

stateBlock
  :: QName
  -> State
  -> State
stateBlock n (State rs ms ss h)
  = State rs (Map.insert n Blocked ms) ss h

stateCheck
  :: QName
  -> Context
  -> State
  -> State
stateCheck n c (State rs ms ss h)
  = State rs (Map.insert n (Checked c) ms) ss h

stateIncrementHash
  :: State
  -> State
stateIncrementHash (State rs ms ss h)
  = State rs ms ss (succ h)

-- ## Get

-- | Get a fresh hash.
getHash
  :: MonadState State m
  => m ModuleNameHash
getHash = do
  hash
    <- gets stateHash
  _
    <- modify stateIncrementHash
  pure (ModuleNameHash hash)

-- | Get the state of a module.
getModule
  :: MonadState State m
  => QName
  -> m (Maybe ModuleState)
getModule n
  = gets (stateModule n)

-- | Get the cache of source paths.
getSources
  :: MonadState State m
  => m ModuleToSource
getSources
  = gets stateSources'

-- ## Modify

-- | Record a new unused item.
modifyInsert
  :: MonadReader Environment m
  => MonadState State m
  => Range
  -> RangeInfo
  -> m ()
modifyInsert r i
  = askSkip >>= flip unless (modify (stateInsert r i))

-- | Mark a list of items as used.
modifyDelete
  :: MonadReader Environment m
  => MonadState State m
  => Set Range
  -> m ()
modifyDelete rs
  = askSkip >>= flip unless (modify (stateDelete rs))

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

-- | Update the cache of sources.
modifySources
  :: MonadState State m
  => ModuleToSource
  -> m ()
modifySources ss
  = modify (stateSources ss)

