{- |
Module: Agda.Unused

Definitions and interface for the 'Unused' type, which represents a collection
of unused Agda code structures.
-}
module Agda.Unused
  ( Unused(..)
  , UnusedItems(..)
  , UnusedOptions(..)
  ) where

import Agda.Unused.Types.Range
  (Range, RangeInfo)

import Data.Text
  (Text)

-- ## Types

-- | A collection of unused items and files.
data Unused
  = Unused
  { unusedFiles
    :: [FilePath]
  , unusedItems
    :: UnusedItems
  } deriving Show

-- | A collection of unused items.
newtype UnusedItems
  = UnusedItems
  { unusedItemsList
    :: [(Range, RangeInfo)]
  } deriving Show

-- | Options required by check functions.
data UnusedOptions
  = UnusedOptions
  { unusedOptionsInclude
    :: [FilePath]
  , unusedOptionsLibraries
    :: [Text]
  , unusedOptionsLibrariesFile
    :: Maybe FilePath
  , unusedOptionsUseLibraries
    :: Bool
  , unusedOptionsUseDefaultLibraries
    :: Bool
  } deriving Show

