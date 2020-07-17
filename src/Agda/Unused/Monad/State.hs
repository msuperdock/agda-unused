module Agda.Unused.Monad.State
  ( ModuleState (..)
  , State (..)
  , modifyDelete
  , modifyDeleteRoots
  , modifyInsert
  , stateCheck
  , stateBlock
  , stateEmpty
  , stateLookup
  ) where

import Agda.Unused.Monad.Context
  (Context)
import Agda.Unused.Monad.Reader
  (Environment, askBuiltin, askRange)
import Agda.Unused.Monad.Writer
  (Log (..), tellLog)
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
import Control.Monad.Writer
  (MonadWriter)
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
  => MonadWriter [Log] m
  => Range
  -> RangeInfo
  -> m ()
modifyInsert r i
  = askBuiltin
  >>= \b -> askRange
  >>= \r' -> modifyInsertWith b r' r i

modifyInsertWith
  :: MonadState State m
  => MonadWriter [Log] m
  => Bool
  -- ^ Whether we are in a builtin module.
  -> Maybe Range
  -- ^ The range to log.
  -> Range
  -> RangeInfo
  -> m ()
modifyInsertWith True _ _ _
  = pure ()
modifyInsertWith False (Just r) r' i | r == r'
  = modify (stateInsert r' i) >> tellLog StateInsert
modifyInsertWith False _ r i
  = modify (stateInsert r i)

modifyDelete
  :: MonadReader Environment m
  => MonadState State m
  => MonadWriter [Log] m
  => Range
  -- ^ The current range.
  -> [Range]
  -- ^ The ranges to delete.
  -> m ()
modifyDelete r rs
  = askBuiltin
  >>= \b -> askRange
  >>= \r' -> modifyDeleteWith b r' (Just r) rs

modifyDeleteRoots
  :: MonadReader Environment m
  => MonadState State m
  => MonadWriter [Log] m
  => [Range]
  -- ^ The ranges to delete.
  -> m ()
modifyDeleteRoots rs
  = askBuiltin
  >>= \b -> askRange
  >>= \r -> modifyDeleteWith b r Nothing rs

modifyDeleteWith
  :: MonadState State m
  => MonadWriter [Log] m
  => Bool
  -- ^ Whether we are in a builtin module.
  -> Maybe Range
  -- ^ The range to log.
  -> Maybe Range
  -- ^ The current range.
  -> [Range]
  -- ^ The ranges to delete.
  -> m ()
modifyDeleteWith True _ _ _
  = pure ()
modifyDeleteWith False (Just r) r' rs | elem r rs
  = modify (stateDelete rs) >> tellLog (maybe StateDeleteRoots StateDelete r')
modifyDeleteWith False _ _ rs
  = modify (stateDelete rs)

