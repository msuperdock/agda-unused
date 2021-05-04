module Main where

import Agda.Unused
  (UnusedOptions(..))
import Agda.Unused.Check
  (checkUnused, checkUnusedGlobal)
import Agda.Unused.Monad.Error
  (Error)
import Agda.Unused.Print
  (printError, printNothing, printUnused, printUnusedItems)

import Control.Monad
  (unless)
import Control.Monad.Except
  (MonadError, runExceptT, throwError)
import Control.Monad.IO.Class
  (MonadIO, liftIO)
import Data.Aeson
  (Value(..), (.=), object)
import Data.Aeson.Text
  (encodeToLazyText)
import Data.Text
  (Text)
import qualified Data.Text
  as T
import qualified Data.Text.IO
  as I
import Data.Text.Lazy
  (toStrict)
import Options.Applicative
  (InfoMod, Parser, ParserInfo, execParser, fullDesc, header, help, helper,
    hidden, info, long, many, metavar, optional, progDesc, short, strArgument,
    strOption, switch)
import System.Directory
  (doesDirectoryExist, doesFileExist, makeAbsolute)
import System.Exit
  (exitFailure, exitSuccess)
import System.IO
  (stderr)

-- ## Options

data Options
  = Options
  { optionsFile
    :: !FilePath
    -- ^ Path of the file to check.
  , optionsGlobal
    :: !Bool
    -- ^ Whether to check project globally.
  , optionsJSON
    :: !Bool
    -- ^ Whether to format output as JSON.
  , optionsInclude
    :: ![FilePath]
    -- ^ Include paths.
  , optionsLibraries
    :: ![Text]
    -- ^ Libraries.
  , optionsLibrariesFile
    :: Maybe FilePath
    -- ^ Alternate libraries file.
  , optionsNoLibraries
    :: Bool
    -- ^ Whether to not use any library files.
  , optionsNoDefaultLibraries
    :: Bool
    -- ^ Whether to not use default libraries.
  } deriving Show

-- Convert options; print error message & exit on failure.
optionsUnused
  :: Options
  -> IO (FilePath, UnusedOptions)
optionsUnused opts
  = runExceptT (optionsUnused' opts)
  >>= optionsUnusedEither

optionsUnusedEither
  :: Either OptionsError (FilePath, UnusedOptions)
  -> IO (FilePath, UnusedOptions)
optionsUnusedEither (Left e)
  = I.hPutStrLn stderr (printOptionsError e) >> exitFailure
optionsUnusedEither (Right opts)
  = pure opts

optionsUnused'
  :: MonadError OptionsError m
  => MonadIO m
  => Options
  -> m (FilePath, UnusedOptions)
optionsUnused' opts = do
  filePath
    <- validateFile (optionsFile opts)
  includePaths
    <- traverse validateDirectory (optionsInclude opts)
  libraryPath
    <- traverse validateFile (optionsLibrariesFile opts)
  pure
    $ (,) filePath
    $ UnusedOptions
    { unusedOptionsInclude
      = includePaths
    , unusedOptionsLibraries
      = optionsLibraries opts
    , unusedOptionsLibrariesFile
      = libraryPath
    , unusedOptionsUseLibraries
      = not (optionsNoLibraries opts)
    , unusedOptionsUseDefaultLibraries
      = not (optionsNoDefaultLibraries opts)
    }

optionsParser
  :: Parser Options
optionsParser
  = Options
  <$> (strArgument
    $ metavar "FILE")
  <*> (switch
    $ short 'g'
    <> long "global"
    <> help "Check project globally")
  <*> (switch
    $ short 'j'
    <> long "json"
    <> help "Format output as JSON")
  <*> many (strOption
    $ short 'i'
    <> long "include-path"
    <> metavar "DIR"
    <> help "Look for imports in DIR"
    <> hidden)
  <*> many (strOption
    $ short 'l'
    <> long "library"
    <> metavar "LIB"
    <> help "Use library LIB"
    <> hidden)
  <*> optional (strOption
    $ long "library-file"
    <> metavar "FILE"
    <> help "Use FILE instead of the standard libraries file"
    <> hidden)
  <*> (switch
    $ long "no-libraries"
    <> help "Don't use any library files"
    <> hidden)
  <*> (switch
    $ long "no-default-libraries"
    <> help "Don't use default libraries"
    <> hidden)

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

-- ## Validate

data OptionsError where

  ErrorFile
    :: FilePath
    -> OptionsError

  ErrorDirectory
    :: FilePath
    -> OptionsError

  deriving Show

printOptionsError
  :: OptionsError
  -> Text
printOptionsError (ErrorFile p)
  = "Error: File not found " <> parens (T.pack p) <> "."
printOptionsError (ErrorDirectory p)
  = "Error: Directory not found " <> parens (T.pack p) <> "."

parens
  :: Text
  -> Text
parens t
  = "(" <> t <> ")"

validateFile
  :: MonadError OptionsError m
  => MonadIO m
  => FilePath
  -> m FilePath
validateFile p = do
  exists
    <- liftIO (doesFileExist p)
  _
    <- unless exists (throwError (ErrorFile p))
  filePath
    <- liftIO (makeAbsolute p)
  pure filePath

validateDirectory
  :: MonadError OptionsError m
  => MonadIO m
  => FilePath
  -> m FilePath
validateDirectory p = do
  exists
    <- liftIO (doesDirectoryExist p)
  _
    <- unless exists (throwError (ErrorDirectory p))
  filePath
    <- liftIO (makeAbsolute p)
  pure filePath

-- ## Check

check
  :: Options
  -> IO ()
check opts = do
  (filePath, opts')
    <- optionsUnused opts
  _
    <- checkWith opts' filePath (optionsGlobal opts) (optionsJSON opts)
  pure ()

checkWith
  :: UnusedOptions
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
  >>= printResult j printUnusedItems
checkWith opts p True j
  = checkUnusedGlobal opts p
  >>= printResult j printUnused

-- ## Print

printResult
  :: Bool
  -- ^ Whether to output JSON.
  -> (a -> Maybe Text)
  -> Either Error a
  -> IO ()
printResult False _ (Left e)
  = I.hPutStrLn stderr (printError e) >> exitFailure
printResult False p (Right x)
  = I.putStrLn (maybe printNothing id (p x)) >> exitSuccess
printResult True p x
  = I.putStrLn (toStrict (encodeToLazyText (printResultJSON p x)))

printResultJSON
  :: (a -> Maybe Text)
  -> Either Error a
  -> Value
printResultJSON _ (Left e)
  = encodeMessage "error" (printError e)
printResultJSON p (Right u)
  = maybe (encodeMessage "none" printNothing) (encodeMessage "unused") (p u)

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

-- ## Main

main
  :: IO ()
main
  = execParser options
  >>= check

