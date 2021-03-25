module Main where

import Agda.Unused.Check
  (checkUnused, checkUnusedGlobal)
import qualified Agda.Unused.Monad.Error
  as E
import qualified Agda.Unused.Options
  as O
import qualified Agda.Unused.Print
  as P

import Data.Aeson
  (Value(..), (.=), object)
import Data.Aeson.Text
  (encodeToLazyText)
import Data.Text
  (Text)
import qualified Data.Text.IO
  as I
import Data.Text.Lazy
  (toStrict)
import Options.Applicative
  (InfoMod, Parser, ParserInfo, execParser, fullDesc, header, help, helper,
    info, long, many, metavar, optional, progDesc, short, strArgument,
    strOption, switch)
import System.Directory
  (listDirectory, makeAbsolute)
import System.Exit
  (exitFailure, exitSuccess)
import System.FilePath
  (isExtensionOf, takeDirectory)
import System.IO
  (stderr)

-- ## Options

data Options
  = Options
  { optionsFile
    :: !FilePath
    -- ^ Path of the file to check.
  , optionsRoot
    :: !(Maybe FilePath)
    -- ^ Path of the project root directory.
  , optionsIncludes
    :: ![FilePath]
    -- ^ Include paths.
  , optionsGlobal
    :: !Bool
    -- ^ Whether to check project globally.
  , optionsJSON
    :: !Bool
    -- ^ Whether to format output as JSON.
  } deriving Show

optionsParser
  :: Parser Options
optionsParser
  = Options
  <$> (strArgument
    $ metavar "FILE"
    <> help "Path of file to check")
  <*> optional (strOption
    $ short 'r'
    <> long "root"
    <> metavar "ROOT"
    <> help "Path of project root directory")
  <*> many (strOption
    $ short 'i'
    <> long "include-path"
    <> metavar "DIR"
    <> help "Look for imports in DIR")
  <*> (switch
    $ short 'g'
    <> long "global"
    <> help "Check project globally")
  <*> (switch
    $ short 'j'
    <> long "json"
    <> help "Format output as JSON")

optionsInfo
  :: InfoMod a
optionsInfo
  = fullDesc
  <> progDesc "Check for unused code in FILE"
  <> header "agda-unused - check for unused code in an Agda project"

options
  :: ParserInfo Options
options
  = info (helper <*> optionsParser) optionsInfo

-- ## Check

check
  :: Options
  -> IO ()
check (Options f r ps g j) = do
  filePath
    <- makeAbsolute f
  rootPath
    <- getRootDirectory r filePath
  includePaths
    <- traverse makeAbsolute ps
  _
    <- checkWith (O.Options rootPath includePaths) filePath g j
  pure ()

checkWith
  :: O.Options
  -- ^ Options to use.
  -> FilePath
  -- ^ Absolute path of the file to check.
  -> Bool
  -- ^ Whether to check project globally.
  -> Bool
  -- ^ Whether to format output as JSON.
  -> IO ()
checkWith opts p False j
  = checkUnused opts p
  >>= printResult j P.printUnusedItems
checkWith opts p True j
  = checkUnusedGlobal opts p
  >>= printResult j P.printUnused

-- ## Print

printResult
  :: Bool
  -- ^ Whether to output JSON.
  -> (a -> Maybe Text)
  -> Either E.Error a
  -> IO ()
printResult False _ (Left e)
  = I.hPutStrLn stderr (P.printError e) >> exitFailure
printResult False p (Right x)
  = I.putStrLn (maybe P.printNothing id (p x)) >> exitSuccess
printResult True p x
  = I.putStrLn (toStrict (encodeToLazyText (printResultJSON p x)))

printResultJSON
  :: (a -> Maybe Text)
  -> Either E.Error a
  -> Value
printResultJSON _ (Left e)
  = encodeMessage "error" (P.printError e)
printResultJSON p (Right u)
  = maybe (encodeMessage "none" P.printNothing) (encodeMessage "unused") (p u)

encodeMessage
  :: Text
  -- ^ Type of message.
  -> Text
  -- ^ Contents of message.
  -> Value
encodeMessage t m
  = object
  [ "type"
    .= t
  , "message"
    .= m
  ]

-- ## Root

getRootDirectory
  :: Maybe FilePath
  -- ^ Path of the project root.
  -> FilePath
  -- ^ Absolute path of the file to check.
  -> IO FilePath
getRootDirectory Nothing p
  = getRootDirectoryFrom (takeDirectory p) (takeDirectory p)
getRootDirectory (Just r) _
  = makeAbsolute r

-- Search recursively upwards for project root directory.
getRootDirectoryFrom
  :: FilePath
  -- ^ Default directory.
  -> FilePath
  -- ^ Starting directory.
  -> IO FilePath
getRootDirectoryFrom d p
  = listDirectory p
  >>= pure . any (isExtensionOf "agda-lib")
  >>= getRootDirectoryWith d p

getRootDirectoryWith
  :: FilePath
  -- ^ Default directory.
  -> FilePath
  -- ^ Starting directory.
  -> Bool
  -- ^ Whether the starting directory contains an .agda-lib file.
  -> IO FilePath
getRootDirectoryWith _ p True
  = pure p
getRootDirectoryWith d p False | takeDirectory p == p
  = pure d
getRootDirectoryWith d p False
  = getRootDirectoryFrom d (takeDirectory p)

-- ## Main

main
  :: IO ()
main
  = execParser options
  >>= check

