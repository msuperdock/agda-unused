{-# LANGUAGE UndecidableInstances #-}

{- |
Module: Agda.Unused.Monad.Error

An error monad for determining unused code.
-}
module Agda.Unused.Monad.Error

  ( -- * Definitions
    
    Error(..)
  , InternalError(..)
  , UnexpectedError(..)
  , UnsupportedError(..)

    -- * Lift

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

-- | An error encountered while checking for unused code.
data Error where

  -- | Ambiguous lookup.
  ErrorAmbiguous
    :: !Range
    -> !QName
    -> Error

  -- | Cyclic module dependency.
  ErrorCyclic
    :: !(Maybe Range)
    -> !QName
    -> Error

  -- | Agda declaration exception.
  ErrorDeclaration
    :: !DeclarationException
    -> Error

  -- | File not found.
  ErrorFile
    :: !(Maybe Range)
    -> !QName
    -> !FilePath
    -> Error

  -- | Agda fixity exception.
  ErrorFixity
    :: !(Maybe Range)
    -> Error

  -- | Internal error; should be reported.
  ErrorInternal
    :: !InternalError
    -> !Range
    -> Error

  -- | Module not found in open statement.
  ErrorOpen
    :: !Range
    -> !QName
    -> Error

  -- | Agda parse error.
  ErrorParse
    :: !ParseError
    -> Error
  
  -- | Agda polarity error.
  ErrorPolarity
    :: !(Maybe Range)
    -> Error

  -- | Root not found.
  ErrorRoot
    :: !QName
    -> !QName
    -> Error

  -- | Unsupported language feature.
  ErrorUnsupported
    :: !UnsupportedError
    -> !Range
    -> Error

  deriving Show

-- | An internal error, indicating a bug in our code. This type of error should
-- be reported by filing an issue.
data InternalError where

  -- | Unexpected declaration type for constructor.
  ErrorConstructor
    :: InternalError

  -- | Unexpected arguments to SectionApp constructor.
  ErrorMacro
    :: InternalError

  -- | Unexpected underscore as name.
  ErrorName
    :: InternalError

  -- | Unexpected name-module mismatch in renaming statement.
  ErrorRenaming
    :: InternalError

  -- | Unexpected data type constructor.
  ErrorUnexpected
    :: !UnexpectedError
    -> InternalError

  deriving Show

-- | An error indicating that a constructor for a data type is used where it
-- should not be used.
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

-- | An error indicating that an unsupported language was found.
data UnsupportedError where

  -- | Record module instance applications.
  UnsupportedMacro
    :: UnsupportedError

  -- | Unquoting primitives.
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

-- ## Lift

-- | Lift a lookup result to our error monad.
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

