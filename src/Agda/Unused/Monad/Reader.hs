{- |
Module: Agda.Unused.Monad.Reader

A reader monad for determining unused code.
-}
module Agda.Unused.Monad.Reader

  ( -- * Definition

    Mode(..)
  , Environment(..)

    -- * Ask

  , askSkip
  , askLocal
  , askGlobalMain
  , askRoot
  , askIncludes

    -- * Local

  , localSkip
  , localGlobal

  ) where

import Agda.Utils.FileName
  (AbsolutePath)
import Control.Monad.Reader
  (MonadReader, ask, local)

-- ## Definition

-- | A type indicating how checking should be done.
data Mode where

  -- | Check nothing.
  Skip
    :: Mode

  -- | Check private items only.
  Local
    :: Mode

  -- | Check all items.
  Global
    :: Mode

  -- | Check imports & reject all other declarations.
  GlobalMain
    :: Mode

  deriving (Eq, Show)

-- | An environment type for use in a reader monad.
data Environment
  = Environment
  { environmentMode
    :: !Mode
    -- ^ The current check mode.
  , environmentRoot
    :: !FilePath
    -- ^ The project root path.
  , environmentIncludes
    :: ![AbsolutePath]
    -- ^ The include paths.
  } deriving Show

-- ## Ask

askMode
  :: MonadReader Environment m
  => m Mode
askMode
  = environmentMode <$> ask

-- | Ask whether to skip checking names.
askSkip
  :: MonadReader Environment m
  => m Bool
askSkip
  = (== Skip) <$> askMode

-- | Ask whether we are in local mode.
askLocal
  :: MonadReader Environment m
  => m Bool
askLocal
  = (== Local) <$> askMode

-- | Ask whether we are in global main mode.
askGlobalMain
  :: MonadReader Environment m
  => m Bool
askGlobalMain
  = (== GlobalMain) <$> askMode

-- | Ask for the project root path.
askRoot
  :: MonadReader Environment m
  => m FilePath
askRoot
  = environmentRoot <$> ask

-- | Ask for the include paths.
askIncludes
  :: MonadReader Environment m
  => m [AbsolutePath]
askIncludes
  = environmentIncludes <$> ask

-- ## Local

localMode
  :: MonadReader Environment m
  => Mode
  -> m a
  -> m a
localMode m
  = local (\e -> e {environmentMode = m})

-- | Perform a local computation, but skip checking names.
localSkip
  :: MonadReader Environment m
  => m a
  -> m a
localSkip
  = localMode Skip

-- | Perform a local computation in global mode.
localGlobal
  :: MonadReader Environment m
  => m a
  -> m a
localGlobal
  = localMode Global

