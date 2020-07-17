module Main where

import Agda.Unused
  (checkUnused)
import Agda.Unused.Print
  (printUnused)
import Agda.Unused.Types.Name
  (Name (..), NamePart (..), QName (..))
import Agda.Unused.Types.Root
  (Root (..))
import qualified Data.Text.IO
  as I

path
  :: FilePath
path
  = "/data/code/prover/src"

-- roots
--   :: [Root]
-- roots
--   = [ Root
--       { rootFile
--         = Qual (Name [Id "Prover"])
--         $ Qual (Name [Id "Prelude"])
--         $ QName (Name [Id "Any"])
--       , rootNames
--         = []
--       }
--     ]

roots
  :: [Root]
roots
  = [ Root
      { rootFile
        = QName (Name [Id "Main"])
      , rootNames
        = [QName (Name [Id "main"])]
      }
    , Root
      { rootFile
        = Qual (Name [Id "Prover"])
        $ Qual (Name [Id "Editor"])
        $ Qual (Name [Id "Indexed"])
        $ QName (Name [Id "Map"])
      , rootNames
        = []
      }
    , Root
      { rootFile
        = Qual (Name [Id "Prover"])
        $ Qual (Name [Id "Editor"])
        $ Qual (Name [Id "Indexed"])
        $ QName (Name [Id "Product"])
      , rootNames
        = []
      }
    , Root
      { rootFile
        = Qual (Name [Id "Prover"])
        $ Qual (Name [Id "Editor"])
        $ Qual (Name [Id "Indexed"])
        $ QName (Name [Id "Sigma"])
      , rootNames
        = []
      }
    ]

main
  :: IO ()
main
  = checkUnused path roots
  >>= I.putStrLn . printUnused

