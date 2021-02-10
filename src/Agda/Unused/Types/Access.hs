{- |
Module: Agda.Unused.Types.Access

Access modifiers indicating whether an item is public or private.
-}
module Agda.Unused.Types.Access

  ( -- * Definition
    
    Access(..)

    -- * Interface

  , access
   
    -- * Conversion

  , fromAccess

  ) where

import qualified Agda.Syntax.Common
  as C

-- ## Definition

-- | An access modifier.
data Access where

  Private
    :: Access

  Public
    :: Access

  deriving Show

-- ## Interface

-- | Elimination rule for 'Access'.
access
  :: a
  -> a
  -> Access
  -> a
access x _ Private
  = x
access _ y Public
  = y

-- ## Conversion

-- | Conversion from Agda access type.
fromAccess
  :: C.Access
  -> Access
fromAccess (C.PrivateAccess _)
  = Private
fromAccess C.PublicAccess
  = Public

