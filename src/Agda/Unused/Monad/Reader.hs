{- |
Module: Agda.Unused.Monad.Reader

A reader monad for determining unused code.
-}
module Agda.Unused.Monad.Reader

  ( -- * Definition
    
    Environment(..)

    -- * Ask

  , askSkip
  , askLocal
  , askRoot

    -- * Local

  , localSkip

  ) where

import Control.Monad.Reader
  (MonadReader, ask, local)

-- | An environment type for use in a reader monad.
data Environment
  = Environment
  { environmentSkip
    :: !Bool
    -- ^ Whether to skip all names.
  , environmentLocal
    :: !Bool
    -- ^ Whether to skip public names.
  , environmentRoot
    :: !FilePath
    -- ^ The project root path.
  } deriving Show

-- | Ask whether to skip checking names.
askSkip
  :: MonadReader Environment m
  => m Bool
askSkip
  = environmentSkip <$> ask

-- | Ask whether to skip checking public names.
askLocal
  :: MonadReader Environment m
  => m Bool
askLocal
  = environmentLocal <$> ask

-- | Ask for the project root path.
askRoot
  :: MonadReader Environment m
  => m FilePath
askRoot
  = environmentRoot <$> ask

-- | Skip checking names in a local computation.
localSkip
  :: MonadReader Environment m
  => m a
  -> m a
localSkip
  = local (\e -> e {environmentSkip = True})

