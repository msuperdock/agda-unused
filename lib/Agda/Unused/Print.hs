module Agda.Unused.Print
  ( printUnused
  ) where

import Agda.Unused.Error
  (Error (..), InternalError (..), UnexpectedError (..), UnsupportedError (..))
import Agda.Unused.Name
  (Name (..), NamePart (..), QName (..))
import Agda.Unused.Range
  (Range, RangeInfo (..), RangeType (..), getRange)

import Agda.Utils.Pretty
  (prettyShow)
import Data.Map.Strict
  (Map)
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
printRange
  = T.pack . show

-- ## Messages

printMessage
  :: Text
  -> Text
  -> Text
printMessage t1 t2
  = T.unlines [t1, indent t2]

-- ## Errors

printError
  :: Error
  -> Text

printError (ErrorAmbiguous r n)
  = printMessage (printRange r)
  $ "error: ambiguous name " <> parens (printQName n)
printError (ErrorCyclic r n)
  = printMessage (maybe (printQName n) printRange r)
  $ "error: cyclic module dependency " <> parens (printQName n)
printError (ErrorFile r n p)
  = printMessage (maybe (printQName n) printRange r)
  $ "error: file not found " <> parens (T.pack p)
printError (ErrorFixity (Just r))
  = printMessage (printRange r)
  $ "error: multiple fixity declarations"
printError (ErrorInternal e r)
  = printMessage (printRange r)
  $ "internal error: " <> printInternalError e
printError (ErrorOpen r n)
  = printMessage (printRange r)
  $ "error: module " <> parens (printQName n) <> " not found"
printError (ErrorPolarity (Just r))
  = printMessage (printRange r)
  $ "error: multiple polarity declarations"
printError (ErrorUnsupported e r)
  = printMessage (printRange r)
  $ "error: " <> printUnsupportedError e <> " not supported"

printError (ErrorFixity Nothing)
  = "error: multiple fixity declarations"
printError (ErrorPolarity Nothing)
  = "error: multiple polarity declarations"

printError (ErrorDeclaration e)
  = printRange (getRange e) <> "\n" <> T.pack (prettyShow e)
printError (ErrorParse e)
  = T.pack (show e)

printInternalError
  :: InternalError
  -> Text
printInternalError ErrorConstructor
  = "invalid data constructor"
printInternalError ErrorName
  = "invalid name"
printInternalError ErrorRenaming
  = "invalid renaming directive"
printInternalError (ErrorUnexpected e)
  = "unexpected constructor " <> quote (printUnexpectedError e)

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
  = "module assignment"
printUnsupportedError UnsupportedUnquote
  = "unquoting primitives"

-- ## Warnings

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

printRangeInfo
  :: RangeInfo
  -> Text
printRangeInfo (RangeInfo t n)
  = T.unwords ["unused", printRangeType t, quote (printQName n)]

printWarning
  :: Range
  -> RangeInfo
  -> Text
printWarning r i
  = printMessage (printRange r) (printRangeInfo i)

printWarnings
  :: Map Range RangeInfo
  -> Text
printWarnings rs | Map.null rs
  = T.unlines ["no unused code"]
printWarnings rs
  = Map.foldMapWithKey printWarning rs

-- ## Main

printUnused
  :: Either Error (Map Range RangeInfo)
  -> Text
printUnused (Left e)
  = printError e
printUnused (Right rs)
  = printWarnings rs

