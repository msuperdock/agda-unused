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
    info, long, metavar, optional, progDesc, short, strOption)
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

check' o@(Options _ Nothing) = do
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
    <- liftIO (I.putStr (either P.printError P.printUnused checkResult))
  pure ()

check' o@(Options _ (Just f)) = do
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

-- ## Root

getRootDirectory
  :: Options
  -> IO FilePath
getRootDirectory (Options (Just p) _)
  = pure p
getRootDirectory (Options Nothing Nothing)
  = getCurrentDirectory >>= \p -> getRootDirectoryFrom p p
getRootDirectory (Options Nothing (Just f))
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

