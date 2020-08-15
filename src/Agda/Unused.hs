{- |
Module: Agda.Unused

Definitions and interface for the 'Unused' type, which represents a collection
of unused Agda code structures.
-}
module Agda.Unused
  ( Unused(..)
  , unusedNull
  ) where

import Agda.Unused.Types.Range
  (Range, RangeInfo)

import Data.Map.Strict
  (Map)
import qualified Data.Map.Strict
  as Map

-- ## Types

-- | A collection of unused items.
newtype Unused
  = Unused
  { unusedMap
    :: Map Range RangeInfo
  } deriving Show

-- | Determine whether the given collection of unused items is empty.
unusedNull
  :: Unused
  -> Bool
unusedNull (Unused rs)
  = Map.null rs

