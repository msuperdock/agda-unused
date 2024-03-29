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
  (RangeInfo(..), RangeType(..))

import Agda.Interaction.FindFile
  (FindError(..))
import Agda.Syntax.Concrete.Definitions.Errors
  (DeclarationException(..))
import Agda.Syntax.Position
  (Range, Range'(..), getRange)
import Agda.Utils.Pretty
  (prettyShow)
import Data.Semigroup
  (sconcat)
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
  = T.pack . prettyShow

printName
  :: Name
  -> Text
printName (Name ps)
  = sconcat (printNamePart <$> ps)

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
  = T.pack (prettyShow r)

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
  = printMessage (printRange r)
  $ "Error: Cyclic module dependency " <> parens (printQName n) <> "."
printError (ErrorFind r n (NotFound _))
  = printMessage (printRange r)
  $ "Error: Import not found " <> parens (printQName n) <> "."
printError (ErrorFind r n (Ambiguous _))
  = printMessage (printRange r)
  $ "Error: Ambiguous import " <> parens (printQName n) <> "."
printError (ErrorFixity (Just r))
  = printMessage (printRange r)
  $ "Error: Multiple fixity declarations."
printError (ErrorGlobal r)
  = printMessage (printRange r)
  $ "Error: With --global, all declarations in the given file must be imports."
printError (ErrorOpen r n)
  = printMessage (printRange r)
  $ "Error: Module not found " <> parens (printQName n) <> "."
printError (ErrorPolarity (Just r))
  = printMessage (printRange r)
  $ "Error: Multiple polarity declarations."
printError (ErrorRoot n n')
  = printMessage (printQName n)
  $ "Error: Root not found " <> parens (quote (printQName n')) <> "."
printError (ErrorUnsupported e r)
  = printMessage (printRange r)
  $ "Error: " <> printUnsupportedError e <> " not supported."

printError (ErrorDeclaration (DeclarationException _ e))
  = printRange (getRange e) <> "\n" <> T.pack (prettyShow e)
printError (ErrorFile p)
  = printErrorFile p
printError (ErrorFixity Nothing)
  = "Error: Multiple fixity declarations."
printError ErrorInclude
  = "Error: Invalid path-related options."
printError (ErrorInternal e)
  = printInternalError e
printError (ErrorParse e)
  = T.pack (prettyShow e)
printError (ErrorPolarity Nothing)
  = "Error: Multiple polarity declarations."

printErrorFile
  :: FilePath
  -> Text
printErrorFile p
  = "Error: File not found " <> parens (T.pack p) <> "."

printInternalError
  :: InternalError
  -> Text
printInternalError (ErrorConstructor r)
  = printMessage (printRange r)
  $ "Internal error: Invalid data constructor."
printInternalError (ErrorLet r)
  = printMessage (printRange r)
  $ "Internal error: Invalid let statement."
printInternalError (ErrorMacro r)
  = printMessage (printRange r)
  $ "Internal error: Invalid module application."
printInternalError (ErrorModuleName n)
  = printMessage (T.pack n)
  $ "Internal error: Empty top-level module name."
printInternalError (ErrorName r)
  = printMessage (printRange r)
  $ "Internal error: Invalid name."
printInternalError (ErrorRenaming r)
  = printMessage (printRange r)
  $ "Internal error: Invalid renaming directive."
printInternalError (ErrorUnexpected e r)
  = printMessage (printRange r)
  $ "Internal error: Unexpected constructor "
    <> quote (printUnexpectedError e) <> "."

printUnexpectedError
  :: UnexpectedError
  -> Text
printUnexpectedError UnexpectedAbsurd
  = "Absurd"
printUnexpectedError UnexpectedAs
  = "As"
printUnexpectedError UnexpectedDontCare
  = "DontCare"
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
printUnsupportedError UnsupportedLoneConstructor
  = "Lone constructors"
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

