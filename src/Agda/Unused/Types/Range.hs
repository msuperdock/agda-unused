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
  , rangeContains

  ) where

import Agda.Unused.Types.Name
  (QName)

import Agda.Syntax.Position
  (PositionWithoutFile, Range, Range'(..), getRange, rEnd', rStart')

-- ## Definitions

-- | The type of item found at a named range.
data RangeType where

  RangeData
    :: RangeType

  RangeDefinition
    :: RangeType

  RangeImport
    :: RangeType

  RangeImportItem
    :: RangeType

  RangeModule
    :: RangeType

  RangeModuleItem
    :: RangeType

  RangeOpen
    :: RangeType

  RangeOpenItem
    :: RangeType

  RangePatternSynonym
    :: RangeType

  RangePostulate
    :: RangeType

  RangeRecord
    :: RangeType

  RangeRecordConstructor
    :: RangeType

  RangeVariable
    :: RangeType

  deriving (Eq, Ord, Show)

-- | Information associated with an item found at a certain range.
data RangeInfo where

  RangeNamed
    :: !RangeType
    -> !QName
    -> RangeInfo

  RangeMutual
    :: RangeInfo

  deriving (Eq, Ord, Show)

-- ## Interface

-- | Determine whether the first range contains the second.
rangeContains
  :: Range
  -> Range
  -> Bool
rangeContains r1@(Range f1 _) r2@(Range f2 _) | f1 == f2
  = rangeContains' (rStart' r1) (rEnd' r1) (rStart' r2) (rEnd' r2)
rangeContains _ _
  = False

rangeContains'
  :: Maybe PositionWithoutFile
  -> Maybe PositionWithoutFile
  -> Maybe PositionWithoutFile
  -> Maybe PositionWithoutFile
  -> Bool
rangeContains' (Just s1) (Just e1) (Just s2) (Just e2)
  = s1 <= s2 && e1 >= e2
rangeContains' _ _ _ _
  = False

