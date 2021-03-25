module Agda.Unused.Options
  ( Options(..)
  , setOptions
  ) where

import Agda.Interaction.Options
  (defaultOptions)
import Agda.Interaction.Options.Lenses
  (setIncludePaths)
import Agda.TypeChecking.Monad.Base
  (TCM)
import Agda.TypeChecking.Monad.Options
  (setCommandLineOptions)

-- | Options required by all check functions in @Agda.Unused.Check@.
data Options
  = Options
  { optionsRoot
    :: FilePath
  , optionsIncludes
    :: [FilePath]
  } deriving Show

-- | Set Agda's internal options.
setOptions
  :: Options
  -> TCM ()
setOptions (Options _ is)
  = setCommandLineOptions
  $ setIncludePaths is
  $ defaultOptions

