{- |
Module: Agda.Unused

Definitions and interface for the 'Unused' type, which represents a collection
of unused Agda code structures.
-}
module Agda.Unused
  ( Unused(..)
  , UnusedItems(..)
  ) where

import Agda.Unused.Types.Range
  (Range, RangeInfo)

import Data.Map.Strict
  (Map)

-- ## Types

-- | A collection of unused items and files.
data Unused
  = Unused
  { unusedItems
    :: UnusedItems
  , unusedPaths
    :: [FilePath]
  } deriving Show

-- | A collection of unused items.
newtype UnusedItems
  = UnusedItems
  { unusedItemsMap
    :: Map Range RangeInfo
  } deriving Show

