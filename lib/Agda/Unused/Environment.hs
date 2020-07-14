module Agda.Unused.Environment
  ( Environment (..)
  , askBuiltin
  , askRoot
  , localBuiltin
  ) where

import Control.Monad.Reader
  (MonadReader, ask, local)

data Environment
  = Environment
  { environmentBuiltin
    :: Bool
  , environmentRoot
    :: FilePath
  }

askRoot
  :: MonadReader Environment m
  => m FilePath
askRoot
  = environmentRoot <$> ask

askBuiltin
  :: MonadReader Environment m
  => m Bool
askBuiltin
  = environmentBuiltin <$> ask

localBuiltin
  :: MonadReader Environment m
  => m a
  -> m a
localBuiltin
  = local (\(Environment _ p) -> Environment True p)

