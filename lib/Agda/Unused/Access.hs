module Agda.Unused.Access
  ( Access (..)
  , access
  , fromAccess
  ) where

import qualified Agda.Syntax.Common
  as C

data Access where

  Private
    :: Access

  Public
    :: Access

  deriving Show

access
  :: a
  -> a
  -> Access
  -> a
access x _ Private
  = x
access _ y Public
  = y

fromAccess
  :: C.Access
  -> Access
fromAccess (C.PrivateAccess _)
  = Private
fromAccess C.PublicAccess
  = Public

