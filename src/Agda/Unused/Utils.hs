{- |
Module: Agda.Unused.Utils

Utility functions for 'Maybe', 'Either', and 'Map' types.
-}
module Agda.Unused.Utils

  ( -- * Maybe

    liftMaybe

    -- * Either

  , mapLeft

    -- * Map

  , mapDeletes
  , mapUpdateKey

  ) where

import Control.Monad.Except
  (MonadError, throwError)
import Data.Map.Strict
  (Map)
import qualified Data.Map.Strict
  as Map

-- ## Maybe

-- | Lift a 'Maybe' type to an error monad by throwing a fixed error.
liftMaybe
  :: MonadError e m
  => e
  -> Maybe a
  -> m a
liftMaybe e Nothing
  = throwError e
liftMaybe _ (Just x)
  = pure x

-- ## Either

-- | Map the left component of an 'Either' type.
mapLeft
  :: (e -> f)
  -> Either e a
  -> Either f a
mapLeft f (Left e)
  = Left (f e)
mapLeft _ (Right x)
  = Right x

-- ## Map

-- | Delete a list of keys from a map.
mapDeletes
  :: Ord k
  => [k]
  -> Map k a
  -> Map k a
mapDeletes ks xs
  = foldr Map.delete xs ks

-- | Modify a key of a map.
--
-- - If the source key is not present, do nothing.
-- - If the target key is already present, overwrite it.
mapUpdateKey
  :: Ord k
  => k
  -> k
  -> Map k a
  -> Map k a
mapUpdateKey k k' m
  = maybe m (\x -> Map.insert k' x (Map.delete k m)) (Map.lookup k m)

