module Main where

import Agda.Unused
  (checkUnused, checkUnusedLocal)
import Agda.Unused.Config
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
import Options.Applicative
  (InfoMod, Parser, ParserInfo, execParser, fullDesc, header, help, helper,
    info, long, metavar, optional, progDesc, short, strOption, value)
import System.Directory
  (doesFileExist, getCurrentDirectory, makeAbsolute)
import System.FilePath
  ((</>), splitDirectories, stripExtension)

-- ## Options

data Options
  = Options
  { optionsRoot
    :: !FilePath
    -- ^ The project root path.
  , optionsFile
    :: Maybe FilePath
    -- ^ A file path to check locally.
  } deriving Show

optionsParser
  :: FilePath
  -- ^ The default file path.
  -> Parser Options
optionsParser p
  = Options
  <$> (strOption
    $ short 'r'
    <> long "root"
    <> metavar "ROOT"
    <> help "Path of project root directory"
    <> value p)
  <*> optional (strOption
    $ short 'l'
    <> long "local"
    <> metavar "FILE"
    <> help "File to check locally")

optionsInfo
  :: InfoMod a
optionsInfo
  = fullDesc
  <> progDesc "Check for unused code in project with root directory ROOT"
  <> header "agda-unused - check for unused code in an Agda project"

options
  :: FilePath
  -- ^ The default file path.
  -> ParserInfo Options
options p
  = info (helper <*> optionsParser p) optionsInfo

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

printError
  :: Error
  -> Text
printError (ErrorFile p)
  = "error: .agda-roots file not found " <> parens (T.pack p)
printError (ErrorLocal l)
  = "error: invalid local path " <> parens (T.pack l)
printError (ErrorParse t)
  = t

parens
  :: Text
  -> Text
parens t
  = "(" <> t <> ")"

-- ## Check

check
  :: Options
  -> IO ()
check p
  = runExceptT (check' p)
  >>= either (I.putStr . printError) (const (pure ()))

check'
  :: MonadError Error m
  => MonadIO m
  => Options
  -> m ()

check' (Options p Nothing) = do
  configPath
    <- pure (p </> ".agda-roots")
  exists
    <- liftIO (doesFileExist configPath)
  _
    <- bool (throwError (ErrorFile configPath)) (pure ()) exists
  contents
    <- liftIO (I.readFile configPath)
  roots
    <- liftEither (mapLeft ErrorParse (parseConfig contents))
  checkResult
    <- liftIO (checkUnused p roots)
  _
    <- liftIO (I.putStr (either P.printError P.printUnused checkResult))
  pure ()

check' (Options p (Just f)) = do
  rootPath
    <- liftIO (makeAbsolute p >>= pure . splitDirectories)
  filePath
    <- liftIO (makeAbsolute f >>= pure . splitDirectories)
  localModule
    <- liftMaybe (ErrorLocal f) (stripPrefix rootPath filePath >>= pathModule)
  checkResult
    <- liftIO (checkUnusedLocal p localModule)
  _
    <- liftIO (I.putStr (either P.printError P.printUnused checkResult))
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

-- ## Main

main
  :: IO ()
main
  = getCurrentDirectory
  >>= execParser . options
  >>= check

