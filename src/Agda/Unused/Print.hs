{- |
Module: Agda.Unused.Print

Printing functions for unused items and errors.
-}
module Agda.Unused.Print
  ( printError
  , printUnused
  ) where

import Agda.Unused
  (Unused(..), unusedNull)
import Agda.Unused.Monad.Error
  (Error(..), InternalError(..), UnexpectedError(..), UnsupportedError(..))
import Agda.Unused.Types.Name
  (Name(..), NamePart(..), QName(..))
import Agda.Unused.Types.Range
  (Range, Range'(..), RangeInfo(..), RangeType(..), getRange)

import Agda.Utils.Pretty
  (prettyShow)
import qualified Data.Map.Strict
  as Map
import Data.Text
  (Text)
import qualified Data.Text
  as T

-- ## Utilities

quote
  :: Text
  -> Text
quote t
  = "‘" <> t <> "’"

parens
  :: Text
  -> Text
parens t
  = "(" <> t <> ")"

indent
  :: Text
  -> Text
indent t
  = "  " <> t

-- ## Names

printNamePart
  :: NamePart
  -> Text
printNamePart
  = T.pack . show

printName
  :: Name
  -> Text
printName (Name ps)
  = mconcat (printNamePart <$> ps)

printQName
  :: QName
  -> Text
printQName (QName n)
  = printName n
printQName (Qual n ns)
  = printName n <> "." <> printQName ns

-- ## Ranges

printRange
  :: Range
  -> Text
printRange NoRange
  = "unknown location"
printRange r@(Range _ _)
  = T.pack (show r)

-- ## Messages

printMessage
  :: Text
  -> Text
  -> Text
printMessage t1 t2
  = T.unlines [t1, t2]

printMessageIndent
  :: Text
  -> Text
  -> Text
printMessageIndent t1 t2
  = T.unlines [t1, indent t2]

-- ## Errors

-- | Print an error.
printError
  :: Error
  -> Text

printError (ErrorAmbiguous r n)
  = printMessage (printRange r)
  $ "Error: Ambiguous name " <> parens (printQName n) <> "."
printError (ErrorCyclic r n)
  = printMessage (maybe (printQName n) printRange r)
  $ "Error: Cyclic module dependency " <> parens (printQName n) <> "."
printError (ErrorFile r n p)
  = printMessage (maybe (printQName n) printRange r)
  $ "Error: File not found " <> parens (T.pack p) <> "."
printError (ErrorFixity (Just r))
  = printMessage (printRange r)
  $ "Error: Multiple fixity declarations."
printError (ErrorInternal e r)
  = printMessage (printRange r)
  $ "Internal error: " <> printInternalError e
printError (ErrorOpen r n)
  = printMessage (printRange r)
  $ "Error: Module not found " <> parens (printQName n) <> "."
printError (ErrorPolarity (Just r))
  = printMessage (printRange r)
  $ "Error: Multiple polarity declarations."
printError (ErrorUnsupported e r)
  = printMessage (printRange r)
  $ "Error: " <> printUnsupportedError e <> " not supported."

printError (ErrorFixity Nothing)
  = "Error: Multiple fixity declarations."
printError (ErrorPolarity Nothing)
  = "Error: Multiple polarity declarations."

printError (ErrorDeclaration e)
  = printRange (getRange e) <> "\n" <> T.pack (prettyShow e)
printError (ErrorParse e)
  = T.pack (show e)

printInternalError
  :: InternalError
  -> Text
printInternalError ErrorConstructor
  = "Invalid data constructor."
printInternalError ErrorName
  = "Invalid name."
printInternalError ErrorRenaming
  = "Invalid renaming directive."
printInternalError (ErrorUnexpected e)
  = "Unexpected constructor " <> quote (printUnexpectedError e) <> "."

printUnexpectedError
  :: UnexpectedError
  -> Text
printUnexpectedError UnexpectedAbsurd
  = "Absurd"
printUnexpectedError UnexpectedAs
  = "As"
printUnexpectedError UnexpectedDontCare
  = "DontCare"
printUnexpectedError UnexpectedETel
  = "ETel"
printUnexpectedError UnexpectedEllipsis
  = "Ellipsis"
printUnexpectedError UnexpectedEqual
  = "Equal"
printUnexpectedError UnexpectedField
  = "Field"
printUnexpectedError UnexpectedNiceFunClause
  = "NiceFunClause"
printUnexpectedError UnexpectedOpApp
  = "OpApp"
printUnexpectedError UnexpectedOpAppP
  = "OpAppP"

printUnsupportedError
  :: UnsupportedError
  -> Text
printUnsupportedError UnsupportedMacro
  = "Module assignments"
printUnsupportedError UnsupportedUnquote
  = "Unquoting primitives"

-- ## Unused

-- | Print a collection of unused ranges.
printUnused
  :: Unused
  -> Text
printUnused u | unusedNull u
  = T.unlines ["No unused code."]
printUnused (Unused rs)
  = Map.foldMapWithKey printRangeInfoWith rs

printRangeInfoWith
  :: Range
  -> RangeInfo
  -> Text
printRangeInfoWith r i
  = printMessageIndent (printRange r) (printRangeInfo i)

printRangeInfo
  :: RangeInfo
  -> Text
printRangeInfo (RangeInfo t n)
  = T.unwords ["unused", printRangeType t, quote (printQName n)]

printRangeType
  :: RangeType
  -> Text
printRangeType RangeData
  = "data type"
printRangeType RangeDefinition
  = "definition"
printRangeType RangeImport
  = "import"
printRangeType RangeImportItem
  = "imported item"
printRangeType RangeOpen
  = "open"
printRangeType RangeOpenItem
  = "opened item"
printRangeType RangePatternSynonym
  = "pattern synonym"
printRangeType RangeRecord
  = "record"
printRangeType RangeRecordConstructor
  = "record constructor"
printRangeType RangeVariable
  = "variable"

