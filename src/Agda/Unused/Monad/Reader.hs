module Agda.Unused.Monad.Reader
  ( Environment (..)
  , askBuiltin
  , askRange
  , askRoot
  , localBuiltin
  ) where

import Agda.Unused.Types.Range
  (Range)

import Control.Monad.Reader
  (MonadReader, ask, local)

data Environment
  = Environment
  { environmentBuiltin
    :: !Bool
  , environmentRoot
    :: !FilePath
  , environmentRange
    :: !(Maybe Range)
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

askRange
  :: MonadReader Environment m
  => m (Maybe Range)
askRange
  = environmentRange <$> ask

localBuiltin
  :: MonadReader Environment m
  => m a
  -> m a
localBuiltin
  = local (\e -> e {environmentBuiltin = True})

