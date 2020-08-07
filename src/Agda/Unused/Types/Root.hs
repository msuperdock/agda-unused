module Agda.Unused.Types.Root
  ( Root(..)
  ) where

import Agda.Unused.Types.Name
  (QName)

data Root
  = Root
  { rootFile
    :: QName
  , rootNames
    :: [QName]
  } deriving Show

