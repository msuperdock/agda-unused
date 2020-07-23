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
import System.Directory
  (doesFileExist, getCurrentDirectory)
import System.FilePath
  ((</>))

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
  = "error: .roots file not found " <> P.parens (T.pack p)
printError (ErrorParse t)
  = t

main'
  :: MonadError Error m
  => MonadIO m
  => FilePath
  -> m ()
main' p = do
  configPath
    <- pure (p </> ".roots")
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

main
  :: IO ()
main
  = runExceptT (main' "/data/code/prover/src")
  >>= either (I.putStr . printError) (const (pure ()))

