module Main where

import Agda.Unused.Config
  (parseConfig)
import Agda.Unused.Types.Name
  (qNamePath)
import Agda.Unused.Types.Root
  (Root(..))
import Agda.Unused.Utils
  (mapLeft)

import Control.Monad.Except
  (MonadError, liftEither, runExceptT, throwError)
import Control.Monad.IO.Class
  (MonadIO, liftIO)
import Data.Bool
  (bool)
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
  ((</>), takeDirectory)

-- ## Options

newtype Options
  = Options
  { optionsRoot
    :: Maybe FilePath
    -- ^ The project root path.
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

optionsInfo
  :: InfoMod a
optionsInfo
  = fullDesc
  <> progDesc "Get root files in project with root directory ROOT"
  <> header "agda-roots - get root files from an .agda-roots file"

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

-- ## Roots

getRoots
  :: Options
  -> IO ()
getRoots o
  = runExceptT (getRoots' o)
  >>= either (I.putStr . printError) (const (pure ()))

getRoots'
  :: MonadError Error m
  => MonadIO m
  => Options
  -> m ()
getRoots' o = do
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
  _
    <- liftIO (I.putStr (printRoots rootPath roots))
  pure ()

-- ## Print

printRoots
  :: FilePath
  -> [Root]
  -> Text
printRoots p rs
  = T.unwords (printRoot p <$> rs)

printRoot
  :: FilePath
  -> Root
  -> Text
printRoot p (Root n _)
  = T.pack (p </> qNamePath n)

printError
  :: Error
  -> Text
printError (ErrorFile p)
  = "Error: .agda-roots file not found " <> parens (T.pack p) <> "."
printError (ErrorParse t)
  = t

parens
  :: Text
  -> Text
parens t
  = "(" <> t <> ")"

-- ## Root

getRootDirectory
  :: Options
  -> IO FilePath
getRootDirectory (Options Nothing)
  = getCurrentDirectory
  >>= \p -> getRootDirectoryFrom p p
getRootDirectory (Options (Just p))
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
  >>= getRoots

