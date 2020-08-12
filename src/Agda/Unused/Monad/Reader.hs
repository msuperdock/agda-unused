{- |
Module: Agda.Unused.Monad.Reader

A reader monad for determining unused code.
-}
module Agda.Unused.Monad.Reader

  ( -- * Definition
    
    Environment(..)

    -- * Ask

  , askBuiltin
  , askRoot

    -- * Local

  , localBuiltin

  ) where

import Control.Monad.Reader
  (MonadReader, ask, local)

-- | An environment type for use in a reader monad.
data Environment
  = Environment
  { environmentBuiltin
    :: !Bool
    -- ^ Whether we are in a builtin module.
  , environmentRoot
    :: !FilePath
    -- ^ The project root path.
  } deriving Show

-- | Ask for the project root path.
askRoot
  :: MonadReader Environment m
  => m FilePath
askRoot
  = environmentRoot <$> ask

-- | Ask whether we are in a builtin module.
askBuiltin
  :: MonadReader Environment m
  => m Bool
askBuiltin
  = environmentBuiltin <$> ask

-- | Perform a computation within a builtin module.
localBuiltin
  :: MonadReader Environment m
  => m a
  -> m a
localBuiltin
  = local (\e -> e {environmentBuiltin = True})

