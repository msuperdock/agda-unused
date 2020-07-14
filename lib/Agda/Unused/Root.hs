module Agda.Unused.Root
  ( Root (..)
  ) where

import Agda.Unused.Name
  (QName)

data Root
  = Root
  { rootFile
    :: QName
  , rootNames
    :: [QName]
  } deriving Show

