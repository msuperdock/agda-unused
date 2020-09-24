{- |
Module: Agda.Unused.Types.Root

A data type representing a public entry point for an Agda project.
-}
module Agda.Unused.Types.Root
  ( Root(..)
  ) where

import Agda.Unused.Types.Name
  (QName)

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

