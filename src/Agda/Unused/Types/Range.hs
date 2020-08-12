{- |
Module: Agda.Unused.Types.Range

Location ranges of Agda code files.
-}
module Agda.Unused.Types.Range

  ( -- * Definitions

    Range
  , Range'(..)
  , RangeType(..)
  , RangeInfo(..)

    -- * Interface

  , getRange

  ) where

import Agda.Unused.Types.Name
  (QName)

import Agda.Syntax.Position
  (Range, Range'(..), getRange)

-- | The type of item found at a range.
data RangeType where

  RangeData
    :: RangeType

  RangeDefinition
    :: RangeType

  RangeImport
    :: RangeType

  RangeImportItem
    :: RangeType

  RangeOpen
    :: RangeType

  RangeOpenItem
    :: RangeType

  RangePatternSynonym
    :: RangeType

  RangeRecord
    :: RangeType

  RangeRecordConstructor
    :: RangeType

  RangeVariable
    :: RangeType

  deriving Show

-- | Information associated with an item found at a certain range.
data RangeInfo
  = RangeInfo
  { rangeType
    :: !RangeType
    -- ^ The type of item.
  , rangeName
    :: !QName
    -- ^ The name of the item.
  } deriving Show

