{- |
Module: Agda.Unused.Print

Printing functions for unused items and errors.
-}
module Agda.Unused.Print
  ( printError
  , printUnused
  , printUnusedItems
  , printNothing
  ) where

import Agda.Unused
  (Unused(..), UnusedItems(..))
import Agda.Unused.Monad.Error
  (Error(..), InternalError(..), UnexpectedError(..), UnsupportedError(..))
import Agda.Unused.Types.Name
  (Name(..), NamePart(..), QName(..))
import Agda.Unused.Types.Range
  (Range, Range'(..), RangeInfo(..), RangeType(..), getRange)

import Agda.Utils.Pretty
  (prettyShow)
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
  = T.intercalate "\n" [t1, t2]

printMessageIndent
  :: Text
  -> Text
  -> Text
printMessageIndent t1 t2
  = T.intercalate "\n" [t1, indent t2]

-- ## Errors

-- | Print an error.
printError
  :: Error
  -> Text

printError (ErrorAmbiguous r n)
  = printMessage (printRange r)
  $ "Error: Ambiguous name " <> parens (quote (printQName n)) <> "."
printError (ErrorCyclic r n)
  = printMessage (maybe (printQName n) printRange r)
  $ "Error: Cyclic module dependency " <> parens (printQName n) <> "."
printError (ErrorFile (Just r) _ p)
  = printMessage (printRange r)
  $ printErrorFile p
printError (ErrorFile _ (Just n) p)
  = printMessage (printQName n)
  $ printErrorFile p
printError (ErrorFixity (Just r))
  = printMessage (printRange r)
  $ "Error: Multiple fixity declarations."
printError (ErrorGlobal r)
  = printMessage (printRange r)
  $ "Error: With --global, all declarations in the given file must be imports."
printError (ErrorInternal e r)
  = printMessage (printRange r)
  $ "Internal error: " <> printInternalError e
printError (ErrorOpen r n)
  = printMessage (printRange r)
  $ "Error: Module not found " <> parens (printQName n) <> "."
printError (ErrorPolarity (Just r))
  = printMessage (printRange r)
  $ "Error: Multiple polarity declarations."
printError (ErrorRoot m n)
  = printMessage (printQName m)
  $ "Error: Root not found " <> parens (quote (printQName n)) <> "."
printError (ErrorUnsupported e r)
  = printMessage (printRange r)
  $ "Error: " <> printUnsupportedError e <> " not supported."

printError (ErrorFile Nothing Nothing p)
  = printErrorFile p
printError (ErrorFixity Nothing)
  = "Error: Multiple fixity declarations."
printError (ErrorPolarity Nothing)
  = "Error: Multiple polarity declarations."

printError (ErrorDeclaration e)
  = printRange (getRange e) <> "\n" <> T.pack (prettyShow e)
printError (ErrorParse e)
  = T.pack (show e)

printErrorFile
  :: FilePath
  -> Text
printErrorFile p
  = "Error: File not found " <> parens (T.pack p) <> "."

printInternalError
  :: InternalError
  -> Text
printInternalError ErrorConstructor
  = "Invalid data constructor."
printInternalError ErrorMacro
  = "Invalid module application."
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
  = "Record module instance applications"
printUnsupportedError UnsupportedUnquote
  = "Unquoting primitives"

-- ## Unused

-- | Print a collection of unused items and files.
printUnused
  :: Unused
  -> Maybe Text
printUnused (Unused ps is)
  = printUnusedWith
    (printUnusedFiles ps)
    (printUnusedItems is)

printUnusedWith
  :: Maybe Text
  -> Maybe Text
  -> Maybe Text
printUnusedWith Nothing Nothing
  = Nothing
printUnusedWith Nothing (Just t2)
  = Just t2
printUnusedWith (Just t1) Nothing
  = Just t1
printUnusedWith (Just t1) (Just t2)
  = Just (T.intercalate "\n" [t1, t2])
    
printUnusedFiles
  :: [FilePath]
  -> Maybe Text
printUnusedFiles []
  = Nothing
printUnusedFiles ps@(_ : _)
  = Just (T.intercalate "\n" (printUnusedFile <$> ps))

printUnusedFile
  :: FilePath
  -> Text
printUnusedFile p
  = printMessageIndent (T.pack p) "unused file"

-- | Print a collection of unused items.
printUnusedItems
  :: UnusedItems
  -> Maybe Text
printUnusedItems (UnusedItems [])
  = Nothing
printUnusedItems (UnusedItems rs@(_ : _))
  = Just (T.intercalate "\n" (uncurry printRangeInfoWith <$> rs))

-- | Print a message indicating that no unused code was found.
printNothing
  :: Text
printNothing
  = "No unused code."

printRangeInfoWith
  :: Range
  -> RangeInfo
  -> Text
printRangeInfoWith r i
  = printMessageIndent (printRange r) (printRangeInfo i)

printRangeInfo
  :: RangeInfo
  -> Text
printRangeInfo (RangeNamed t n)
  = T.unwords ["unused", printRangeType t, quote (printQName n)]
printRangeInfo RangeMutual
  = "unused mutually recursive definition"

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
printRangeType RangeModule
  = "module"
printRangeType RangeModuleItem
  = "module assignment item"
printRangeType RangeOpen
  = "open"
printRangeType RangeOpenItem
  = "opened item"
printRangeType RangePatternSynonym
  = "pattern synonym"
printRangeType RangePostulate
  = "postulate"
printRangeType RangeRecord
  = "record"
printRangeType RangeRecordConstructor
  = "record constructor"
printRangeType RangeVariable
  = "variable"

