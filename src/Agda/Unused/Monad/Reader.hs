module Agda.Unused.Monad.Reader
  ( Environment(..)
  , askBuiltin
  , askRoot
  , localBuiltin
  ) where

import Control.Monad.Reader
  (MonadReader, ask, local)

data Environment
  = Environment
  { environmentBuiltin
    :: !Bool
  , environmentRoot
    :: !FilePath
  } deriving Show

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
  = local (\e -> e {environmentBuiltin = True})

