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

-- | Determine whether the first range contains the second.
rangeContains
  :: Range
  -> Range
  -> Bool
rangeContains r1 r2
  = rangeContains' (rStart' r1) (rEnd' r1) (rStart' r2) (rEnd' r2)

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

