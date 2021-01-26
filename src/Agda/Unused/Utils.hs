{- |
Module: Agda.Unused.Utils

Utility functions for 'Maybe', 'Either', and 'Map' types.
-}
module Agda.Unused.Utils

  ( -- * Maybe

    liftMaybe

    -- * Either

  , mapLeft

    -- * List

  , stripSuffix

    -- * Map

  , mapDeletes

  ) where

import Control.Monad.Except
  (MonadError, throwError)
import Data.List
  (stripPrefix)
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

-- ## List

-- | Drop the given suffix from a list.
stripSuffix
  :: Eq a
  => [a]
  -> [a]
  -> Maybe [a]
stripSuffix xs ys
  = reverse <$> stripPrefix (reverse xs) (reverse ys)

-- ## Map

-- | Delete a list of keys from a map.
mapDeletes
  :: Ord k
  => [k]
  -> Map k a
  -> Map k a
mapDeletes ks xs
  = foldr Map.delete xs ks

