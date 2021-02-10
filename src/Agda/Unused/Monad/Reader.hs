{- |
Module: Agda.Unused.Monad.Reader

A reader monad for determining unused code.
-}
module Agda.Unused.Monad.Reader

  ( -- * Definition
    
    Environment(..)

    -- * Ask

  , askGlobal
  , askRoot
  , askSkip

    -- * Local

  , localSkip

  ) where

import Control.Monad.Reader
  (MonadReader, ask, local)

-- ## Definition

-- | An environment type for use in a reader monad.
data Environment
  = Environment
  { environmentSkip
    :: !Bool
    -- ^ Whether to skip all names.
  , environmentGlobal
    :: !Bool
    -- ^ Whether to perform global check.
  , environmentRoot
    :: !FilePath
    -- ^ The project root path.
  } deriving Show

-- ## Ask

-- | Ask whether to skip checking names.
askSkip
  :: MonadReader Environment m
  => m Bool
askSkip
  = environmentSkip <$> ask

-- | Ask whether to skip checking public names.
askGlobal
  :: MonadReader Environment m
  => m Bool
askGlobal
  = environmentGlobal <$> ask

-- | Ask for the project root path.
askRoot
  :: MonadReader Environment m
  => m FilePath
askRoot
  = environmentRoot <$> ask

-- ## Local

-- | Skip checking names in a local computation.
localSkip
  :: MonadReader Environment m
  => m a
  -> m a
localSkip
  = local (\e -> e {environmentSkip = True})

