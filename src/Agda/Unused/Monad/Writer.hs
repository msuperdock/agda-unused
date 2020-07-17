module Agda.Unused.Monad.Writer
  ( Log (..)
  , tellDeclaration
  , tellLog
  ) where

import Agda.Unused.Monad.Context
  (AccessContext, accessContextSearch)
import Agda.Unused.Monad.Reader
  (Environment, askBuiltin, askRange)
import Agda.Unused.Types.Name
  (Name, QName)
import Agda.Unused.Types.Range
  (Range)

import Control.Monad.Reader
  (MonadReader)
import Control.Monad.Writer
  (MonadWriter, tell)

data Log where

  StateInsert
    :: Log

  StateDelete
    :: !Range
    -> Log

  StateDeleteRoots
    :: Log

  CheckName
    :: !Range
    -> !Name
    -> Log

  CheckDeclaration
    :: !Range
    -> ![QName]
    -> Log

  deriving Show

tellLog
  :: MonadWriter [Log] m
  => Log
  -> m ()
tellLog
  = tell . (: [])

tellDeclaration
  :: MonadReader Environment m
  => MonadWriter [Log] m
  => Range
  -- ^ The current range.
  -> AccessContext
  -> m ()
tellDeclaration r c
  = askBuiltin
  >>= \b -> askRange
  >>= \r' -> tellDeclarationWith b r' r c

tellDeclarationWith
  :: MonadWriter [Log] m
  => Bool
  -> Maybe Range
  -- ^ The range to monitor.
  -> Range
  -- ^ The current range.
  -> AccessContext
  -> m ()
tellDeclarationWith False (Just r) r' c
  = tellLog (CheckDeclaration r' (accessContextSearch r c))
tellDeclarationWith False Nothing _ _
  = pure ()
tellDeclarationWith True _ _ _
  = pure ()

