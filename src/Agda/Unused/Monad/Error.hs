{-# LANGUAGE UndecidableInstances #-}

module Agda.Unused.Monad.Error
  ( Error(..)
  , InternalError(..)
  , LookupError(..)
  , UnexpectedError(..)
  , UnsupportedError(..)
  , liftLookup
  ) where

import Agda.Unused.Types.Context
  (LookupError(..))
import Agda.Unused.Types.Name
  (QName)
import Agda.Unused.Types.Range
  (Range, getRange)

import Agda.Syntax.Concrete.Definitions
  (DeclarationException)
import Agda.Syntax.Concrete.Fixity
  (MonadFixityError(..))
import Agda.Syntax.Parser
  (ParseError)
import Control.Monad.Except
  (MonadError, throwError)

-- ## Definitions

data Error where

  ErrorAmbiguous
    :: !Range
    -> !QName
    -> Error

  ErrorCyclic
    :: !(Maybe Range)
    -> !QName
    -> Error

  ErrorDeclaration
    :: !DeclarationException
    -> Error

  ErrorFile
    :: !(Maybe Range)
    -> !QName
    -> !FilePath
    -> Error

  ErrorFixity
    :: !(Maybe Range)
    -> Error

  ErrorInternal
    :: !InternalError
    -> !Range
    -> Error

  ErrorOpen
    :: !Range
    -> !QName
    -> Error

  ErrorParse
    :: !ParseError
    -> Error
  
  ErrorPolarity
    :: !(Maybe Range)
    -> Error

  ErrorUnsupported
    :: !UnsupportedError
    -> !Range
    -> Error

  deriving Show

data InternalError where

  ErrorConstructor
    :: InternalError

  ErrorName
    :: InternalError

  ErrorRenaming
    :: InternalError

  ErrorUnexpected
    :: !UnexpectedError
    -> InternalError

  deriving Show

data UnexpectedError where

  UnexpectedAbsurd
    :: UnexpectedError

  UnexpectedAs
    :: UnexpectedError

  UnexpectedDontCare
    :: UnexpectedError

  UnexpectedETel
    :: UnexpectedError

  UnexpectedEllipsis
    :: UnexpectedError

  UnexpectedEqual
    :: UnexpectedError

  UnexpectedField
    :: UnexpectedError

  UnexpectedNiceFunClause
    :: UnexpectedError

  UnexpectedOpApp
    :: UnexpectedError

  UnexpectedOpAppP
    :: UnexpectedError

  deriving Show

data UnsupportedError where

  UnsupportedMacro
    :: UnsupportedError

  UnsupportedUnquote
    :: UnsupportedError

  deriving Show

-- ## Fixity

instance (Monad m, MonadError Error m) => MonadFixityError m where
  throwMultipleFixityDecls []
    = throwError (ErrorFixity Nothing)
  throwMultipleFixityDecls ((n, _) : _)
    = throwError (ErrorFixity (Just (getRange n)))
  throwMultiplePolarityPragmas []
    = throwError (ErrorPolarity Nothing)
  throwMultiplePolarityPragmas (n : _)
    = throwError (ErrorPolarity (Just (getRange n)))
  warnUnknownNamesInFixityDecl _
    = pure ()
  warnUnknownNamesInPolarityPragmas _
    = pure ()
  warnUnknownFixityInMixfixDecl _
    = pure ()
  warnPolarityPragmasButNotPostulates _
    = pure ()

-- ## Lookup

liftLookup
  :: MonadError Error m
  => Range
  -> QName
  -> Either LookupError a
  -> m a
liftLookup r n (Left LookupNotFound)
  = throwError (ErrorOpen r n)
liftLookup r n (Left LookupAmbiguous)
  = throwError (ErrorAmbiguous r n)
liftLookup _ _ (Right x)
  = pure x

