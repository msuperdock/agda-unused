module Main where

import Agda.Unused
  (Unused, unusedNull)
import Agda.Unused.Check
  (checkUnused, checkUnusedLocal)
import Agda.Unused.Config
  (parseConfig)
import qualified Agda.Unused.Monad.Error
  as E
import qualified Agda.Unused.Print
  as P
import Agda.Unused.Types.Name
  (Name(..), NamePart(..), QName(..))
import Agda.Unused.Utils
  (liftMaybe, mapLeft)

import Control.Monad.Except
  (MonadError, liftEither, runExceptT, throwError)
import Control.Monad.IO.Class
  (MonadIO, liftIO)
import Data.Aeson
  (Value(..), (.=), object)
import Data.Aeson.Text
  (encodeToLazyText)
import Data.Bool
  (bool)
import Data.List
  (stripPrefix)
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
    info, long, metavar, optional, progDesc, short, strOption, switch)
import System.Directory
  (doesFileExist, getCurrentDirectory, makeAbsolute)
import System.FilePath
  ((</>), splitDirectories, stripExtension, takeDirectory)

-- ## Options

data Options
  = Options
  { optionsRoot
    :: !(Maybe FilePath)
    -- ^ The project root path.
  , optionsFile
    :: !(Maybe FilePath)
    -- ^ A file path to check locally.
  , optionsJSON
    :: !Bool
    -- ^ Whether to output JSON.
  } deriving Show

optionsParser
  :: Parser Options
optionsParser
  = Options
  <$> optional (strOption
    $ short 'r'
    <> long "root"
    <> metavar "ROOT"
    <> help "Path of project root directory")
  <*> optional (strOption
    $ short 'l'
    <> long "local"
    <> metavar "FILE"
    <> help "Path of file to check locally")
  <*> (switch
    $ short 'j'
    <> long "json"
    <> help "Format output as JSON")

optionsInfo
  :: InfoMod a
optionsInfo
  = fullDesc
  <> progDesc "Check for unused code in project with root directory ROOT"
  <> header "agda-unused - check for unused code in an Agda project"

options
  :: ParserInfo Options
options
  = info (helper <*> optionsParser) optionsInfo

-- ## Errors

data Error where
 
  ErrorFile
    :: !FilePath
    -> Error

  ErrorLocal
    :: !FilePath
    -> Error

  ErrorParse
    :: !Text
    -> Error

-- ## Check

check
  :: Options
  -> IO ()
check o@(Options _ _ j)
  = runExceptT (check' o)
  >>= either (I.putStr . printErrorWith j) (const (pure ()))

check'
  :: MonadError Error m
  => MonadIO m
  => Options
  -> m ()

check' o@(Options _ Nothing j) = do
  rootPath
    <- liftIO (getRootDirectory o)
  configPath
    <- pure (rootPath </> ".agda-roots")
  exists
    <- liftIO (doesFileExist configPath)
  _
    <- bool (throwError (ErrorFile configPath)) (pure ()) exists
  contents
    <- liftIO (I.readFile configPath)
  roots
    <- liftEither (mapLeft ErrorParse (parseConfig contents))
  checkResult
    <- liftIO (checkUnused rootPath roots)
  _
    <- liftIO (I.putStr (printResultWith j checkResult))
  pure ()

check' o@(Options _ (Just f) j) = do
  rootPath
    <- liftIO (getRootDirectory o)
  rootPath'
    <- liftIO (makeAbsolute rootPath >>= pure . splitDirectories)
  filePath
    <- liftIO (makeAbsolute f >>= pure . splitDirectories)
  localModule
    <- liftMaybe (ErrorLocal f) (stripPrefix rootPath' filePath >>= pathModule)
  checkResult
    <- liftIO (checkUnusedLocal rootPath localModule)
  _
    <- liftIO (I.putStr (printResultWith j checkResult))
  pure ()

pathModule
  :: [FilePath]
  -> Maybe QName
pathModule []
  = Nothing
pathModule (p : [])
  = QName . name <$> stripExtension "agda" p
pathModule (p : ps@(_ : _))
  = Qual (name p) <$> pathModule ps

name
  :: String
  -> Name
name p
  = Name [Id p]

-- ## Print

printErrorWith
  :: Bool
  -- ^ Whether to output JSON.
  -> Error
  -> Text
printErrorWith False
  = printError
printErrorWith True
  = toStrict . encodeToLazyText . printErrorJSON

printError
  :: Error
  -> Text
printError (ErrorFile p)
  = "Error: .agda-roots file not found " <> parens (T.pack p) <> "."
printError (ErrorLocal l)
  = "Error: Invalid local path " <> parens (T.pack l) <> "."
printError (ErrorParse t)
  = t

printResultWith
  :: Bool
  -- ^ Whether to output JSON.
  -> Either E.Error Unused
  -> Text
printResultWith False
  = printResult
printResultWith True
  = toStrict . encodeToLazyText . printResultJSON

printResult
  :: Either E.Error Unused
  -> Text
printResult
  = either P.printError P.printUnused

parens
  :: Text
  -> Text
parens t
  = "(" <> t <> ")"

-- ## JSON

printErrorJSON
  :: Error
  -> Value
printErrorJSON e
  = encodeMessage "error" (printError e)

printResultJSON
  :: Either E.Error Unused
  -> Value
printResultJSON (Left e)
  = encodeMessage "error" (P.printError e)
printResultJSON (Right u) | unusedNull u
  = encodeMessage "none" (P.printUnused u)
printResultJSON (Right u)
  = encodeMessage "unused" (P.printUnused u)

encodeMessage
  :: Text
  -- ^ Type of message.
  -> Text
  -- ^ Contents of message.
  -> Value
encodeMessage t m
  = object ["type" .= t, "message" .= m]

-- ## Root

getRootDirectory
  :: Options
  -> IO FilePath
getRootDirectory (Options (Just p) _ _)
  = pure p
getRootDirectory (Options Nothing Nothing _)
  = getCurrentDirectory >>= \p -> getRootDirectoryFrom p p
getRootDirectory (Options Nothing (Just f) _)
  = getRootDirectoryFrom (takeDirectory f) (takeDirectory f)

-- Search recursively upwards for project root directory.
getRootDirectoryFrom
  :: FilePath
  -- ^ Default directory.
  -> FilePath
  -- ^ Starting directory.
  -> IO FilePath
getRootDirectoryFrom d p
  = doesFileExist (p </> ".agda-roots") >>= getRootDirectoryWith d p

getRootDirectoryWith
  :: FilePath
  -- ^ Default directory.
  -> FilePath
  -- ^ Starting directory.
  -> Bool
  -- ^ Whether the starting directory contains an .agda-roots file.
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

