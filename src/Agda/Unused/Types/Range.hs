module Agda.Unused.Types.Range
  ( Range
  , Range'(..)
  , RangeType(..)
  , RangeInfo(..)
  , getRange
  ) where

import Agda.Unused.Types.Name
  (QName)

import Agda.Syntax.Position
  (Range, Range'(..), getRange)

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

data RangeInfo
  = RangeInfo
  { rangeType
    :: !RangeType
  , rangeName
    :: !QName
  } deriving Show

