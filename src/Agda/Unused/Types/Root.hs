{- |
Module: Agda.Unused.Types.Root

A data type representing a public entry point for an Agda project, which should
not generate a warning if unused internally.
-}
module Agda.Unused.Types.Root
  ( Root(..)
  ) where

import Agda.Unused.Types.Name
  (QName)

-- | A public entry point for an Agda project, which should not generate a
-- warning if unused internally.
data Root
  = Root
  { rootFile
    :: QName
    -- ^ A module name.
  , rootNames
    :: [QName]
    -- ^ An identifier name, in top-level scope at the end of the module.
  } deriving Show

