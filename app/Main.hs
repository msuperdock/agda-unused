module Main where

import Agda.Unused.Check
  (checkUnused, checkUnusedLocal)
import qualified Agda.Unused.Monad.Error
  as E
import Agda.Unused.Parse
  (parseConfig)
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
import System.Exit
  (exitFailure, exitSuccess)
import System.FilePath
  ((</>), splitDirectories, stripExtension, takeDirectory)
import System.IO
  (stderr)

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

  deriving Show

-- ## Check

check
  :: Options
  -> IO ()
check o@(Options _ _ j)
  = runExceptT (check' o)
  >>= either (printErrorWith j) (const (pure ()))

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
    <- liftIO (printResult j P.printUnused checkResult)
  pure ()

check' o@(Options _ (Just f) j) = do
  rootPath
    <- liftIO (getRootDirectory o)
  rootPath'
    <- pure (splitDirectories rootPath)
  filePath
    <- liftIO (makeAbsolute f >>= \f' -> pure (splitDirectories f'))
  localModule
    <- liftMaybe (ErrorLocal f) (stripPrefix rootPath' filePath >>= pathModule)
  checkResult
    <- liftIO (checkUnusedLocal rootPath localModule)
  _
    <- liftIO (printResult j P.printUnusedItems checkResult)
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

printErrorWith
  :: Bool
  -- ^ Whether to output JSON.
  -> Error
  -> IO ()
printErrorWith False e
  = I.hPutStrLn stderr (printError e) >> exitFailure
printErrorWith True e
  = I.putStrLn (toStrict (encodeToLazyText (printErrorJSON e)))

printError
  :: Error
  -> Text
printError (ErrorFile p)
  = "Error: .agda-roots file not found " <> parens (T.pack p) <> "."
printError (ErrorLocal l)
  = "Error: Invalid local path " <> parens (T.pack l) <> "."
printError (ErrorParse t)
  = t

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
  = object ["type" .= t, "message" .= m]

-- ## Root

getRootDirectory
  :: Options
  -> IO FilePath
getRootDirectory (Options Nothing Nothing _)
  = getCurrentDirectory
  >>= \p -> getRootDirectoryFrom p p
getRootDirectory (Options Nothing (Just p) _)
  = makeAbsolute p
  >>= \p' -> getRootDirectoryFrom (takeDirectory p') (takeDirectory p')
getRootDirectory (Options (Just p) _ _)
  = makeAbsolute p

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

