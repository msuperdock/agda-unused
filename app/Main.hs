module Main where

import Agda.Unused
  (checkUnused)
import Agda.Unused.Config
  (parseConfig)
import qualified Agda.Unused.Print
  as P
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
    info, long, metavar, progDesc, short, strOption, value)
import System.Directory
  (doesFileExist, getCurrentDirectory)
import System.FilePath
  ((</>))

-- ## Options

filePath
  :: FilePath
  -- ^ The default file path.
  -> Parser FilePath
filePath p
  = strOption
  $ short 'r'
  <> long "root"
  <> metavar "ROOT"
  <> help "Path of project root directory"
  <> value p

optionsInfo
  :: InfoMod a
optionsInfo
  = fullDesc
  <> progDesc "Check for unused code in project with root directory ROOT"
  <> header "agda-unused - check for unused code in an Agda project"

options
  :: FilePath
  -- ^ The default file path.
  -> ParserInfo FilePath
options p
  = info (helper <*> filePath p) optionsInfo

-- ## Errors

data Error where
 
  ErrorFile
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
printError (ErrorParse t)
  = t

parens
  :: Text
  -> Text
parens t
  = "(" <> t <> ")"

-- ## Check

check
  :: FilePath
  -> IO ()
check p
  = runExceptT (check' p)
  >>= either (I.putStr . printError) (const (pure ()))

check'
  :: MonadError Error m
  => MonadIO m
  => FilePath
  -> m ()
check' p = do
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

-- ## Main

main
  :: IO ()
main
  = getCurrentDirectory
  >>= execParser . options
  >>= check

