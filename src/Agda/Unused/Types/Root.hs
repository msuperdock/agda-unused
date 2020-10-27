{- |
Module: Agda.Unused.Types.Root

Data types representing public entry points for an Agda project.
-}
module Agda.Unused.Types.Root

  ( -- * Types

    Root(..)
  , Roots(..)

    -- * Construction

  , fromList

  ) where

import Agda.Unused.Types.Name
  (QName)

import Data.Either
  (lefts, rights)

-- | A public entry point for an Agda project.
data Root
  = Root
  { rootFile
    :: QName
    -- ^ A module name.
  , rootNames
    :: Maybe [QName]
    -- ^ Identifier names. An value of 'Nothing' represents all names in scope.
  } deriving Show

-- | A collection of public entry points for an Agda project.
data Roots
  = Roots
  { rootsCheck
    :: [Root]
    -- ^ Modules to check.
  , rootsIgnore
    :: [QName]
    -- ^ Modules to ignore.
  } deriving Show

-- | Construct a collection of roots from a list of elements.
fromList
  :: [Either QName Root]
  -> Roots
fromList rs
  = Roots (rights rs) (lefts rs)

