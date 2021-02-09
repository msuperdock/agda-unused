module Main where

import Agda.Unused.Check
  (checkUnused, checkUnusedLocal)
import qualified Agda.Unused.Monad.Error
  as E
import qualified Agda.Unused.Print
  as P
import Agda.Unused.Types.Name
  (Name(..), NamePart(..), QName(..))
import Agda.Unused.Utils
  (liftMaybe)

import Control.Monad.Except
  (MonadError, runExceptT)
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
    info, long, metavar, optional, progDesc, short, strArgument, strOption,
    switch)
import System.Directory
  (getCurrentDirectory, makeAbsolute)
import System.Exit
  (exitFailure, exitSuccess)
import System.FilePath
  (splitDirectories, stripExtension)
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
  , optionsLocal
    :: !Bool
    -- ^ Whether to ignore publicly accessible items.
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
  <*> (switch
    $ short 'l'
    <> long "local"
    <> help "Ignore publicly accessible items")
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

  ErrorParse
    :: !Text
    -> Error

  deriving Show

-- ## Check

check
  :: Options
  -> IO ()
check o@(Options _ _ _ j)
  = runExceptT (check' o)
  >>= either (printErrorWith j) (const (pure ()))

check'
  :: MonadError Error m
  => MonadIO m
  => Options
  -> m ()

check' (Options f r l j) = do
  rootPath
    <- maybe (liftIO getCurrentDirectory) pure r
  rootPath'
    <- pure (splitDirectories rootPath)
  filePath
    <- liftIO (makeAbsolute f >>= \f' -> pure (splitDirectories f'))
  module'
    <- liftMaybe (ErrorFile f) (stripPrefix rootPath' filePath >>= pathModule)
  _
    <- liftIO (bool checkGlobal checkLocal l j rootPath module')
  pure ()

checkGlobal
  :: Bool
  -- ^ Whether to format output as JSON.
  -> FilePath
  -- ^ The project root path.
  -> QName
  -- ^ The module to check.
  -> IO ()
checkGlobal j p n
  = checkUnused True p n
  >>= printResult j P.printUnused

checkLocal
  :: Bool
  -- ^ Whether to format output as JSON.
  -> FilePath
  -- ^ The project root path.
  -> QName
  -- ^ The module to check.
  -> IO ()
checkLocal j p n
  = checkUnusedLocal p n
  >>= printResult j P.printUnusedItems

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
  = "Error: Invalid local path " <> parens (T.pack p) <> "."
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

-- ## Main

main
  :: IO ()
main
  = execParser options
  >>= check

