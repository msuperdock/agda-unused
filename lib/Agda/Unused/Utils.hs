module Agda.Unused.Utils where

import Control.Monad.Except
  (MonadError, throwError)
import Data.Map.Strict
  (Map)
import qualified Data.Map.Strict
  as Map

-- ## Maybe

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

mapLeft
  :: (e -> f)
  -> Either e a
  -> Either f a
mapLeft f (Left e)
  = Left (f e)
mapLeft _ (Right x)
  = Right x

-- ## Map

mapDeletes
  :: Ord k
  => [k]
  -> Map k a
  -> Map k a
mapDeletes ks xs
  = foldr Map.delete xs ks

mapUpdateKey
  :: Ord k
  => k
  -> k
  -> Map k a
  -> Map k a
mapUpdateKey k k' m
  = maybe m (\x -> Map.insert k' x (Map.delete k m)) (Map.lookup k m)

