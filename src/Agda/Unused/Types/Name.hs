{- |
Module: Agda.Unused.Types.Name

Names and qualified names.
-}
module Agda.Unused.Types.Name

  ( -- * Definitions

    NamePart(..)
  , Name(..)
  , QName(..)

    -- * Interface

  , nameIds
  , stripPrefix

    -- * Conversion

  , fromName
  , fromNameRange
  , fromQName
  , fromQNameRange
  , fromAsName
  , fromModuleName

  , toName
  , toQName

    -- * Paths

  , qNamePath
  , pathQName

    -- * Match

  , matchOperators

  ) where

import Agda.Unused.Utils
  (stripSuffix)

import Agda.Syntax.Concrete
  (AsName, AsName'(..))
import Agda.Syntax.Concrete.Name
  (NameInScope(..), NamePart(..), TopLevelModuleName(..))
import qualified Agda.Syntax.Concrete.Name
  as N
import Agda.Syntax.Position
  (Range, Range'(..))
import Data.List
  (isSubsequenceOf)
import qualified Data.List
  as List
import Data.Maybe
  (mapMaybe)
import System.FilePath
  ((</>), (<.>), splitDirectories)

-- ## Definitions

-- | An unqualified name, represented as a list of name parts.
newtype Name
  = Name
  { nameParts
    :: [NamePart]
  } deriving (Eq, Ord, Show)

-- | A qualified name.
data QName where

  QName
    :: !Name
    -> QName

  Qual
    :: !Name
    -> !QName
    -> QName

  deriving (Eq, Ord, Show)

-- ## Interface

-- | Get the non-hole parts of a 'Name'.
nameIds
  :: Name
  -> [String]
nameIds (Name ps)
  = mapMaybe namePartId ps

namePartId
  :: NamePart
  -> Maybe String
namePartId Hole
  = Nothing
namePartId (Id s)
  = Just s

isOperator
  :: Name
  -> Bool
isOperator (Name [])
  = False
isOperator (Name (_ : []))
  = False
isOperator (Name (_ : _ : _))
  = True

-- | If the first module name is a prefix of the second module name, then strip
-- the prefix, otherwise return 'Nothing'.
stripPrefix
  :: QName
  -- ^ The prefix to strip
  -> QName
  -> Maybe QName
stripPrefix (QName m) (Qual n ns) | m == n
  = Just ns
stripPrefix (Qual m ms) (Qual n ns) | m == n
  = stripPrefix ms ns
stripPrefix _ _
  = Nothing

-- ## Conversion

-- | Conversion from Agda name type.
fromName
  :: N.Name
  -> Maybe Name
fromName (N.NoName _ _)
  = Nothing
fromName (N.Name _ _ n)
  = Just (Name n)

-- | Like 'fromName', but also return a 'Range'.
fromNameRange
  :: N.Name
  -> Maybe (Range, Name)
fromNameRange (N.NoName _ _)
  = Nothing
fromNameRange (N.Name r _ n)
  = Just (r, Name n)

-- | Conversion from Agda qualified name type.
fromQName
  :: N.QName
  -> Maybe QName
fromQName (N.QName n)
  = QName <$> fromName n
fromQName (N.Qual n ns)
  = Qual <$> fromName n <*> fromQName ns

-- | Like 'fromQName', but also return a 'Range'.
fromQNameRange
  :: N.QName
  -> Maybe (Range, QName)
fromQNameRange (N.QName n)
  = fmap QName <$> fromNameRange n
fromQNameRange (N.Qual n ns)
  = fmap . Qual <$> fromName n <*> fromQNameRange ns

-- | Conversion from Agda as-name type.
fromAsName
  :: AsName
  -> Maybe Name
fromAsName (AsName (Left _) _)
  = Nothing
fromAsName (AsName (Right n) _)
  = fromName n

-- | Conversion from Agda top level module name type.
fromModuleName
  :: TopLevelModuleName
  -> Maybe QName
fromModuleName (TopLevelModuleName _ [])
  = Nothing
fromModuleName (TopLevelModuleName _ (n : ns))
  = Just (fromStrings n ns)

fromStrings
  :: String
  -> [String]
  -> QName
fromStrings n []
  = QName (fromString n)
fromStrings n (n' : ns)
  = Qual (fromString n) (fromStrings n' ns)

fromString
  :: String
  -> Name
fromString n
  = Name [Id n]

-- | Conversion to Agda name type.
toName
  :: Name
  -> N.Name
toName (Name n)
  = N.Name NoRange NotInScope n

-- | Conversion to Agda qualified name type.
toQName
  :: QName
  -> N.QName
toQName (QName n)
  = N.QName (toName n)
toQName (Qual n ns)
  = N.Qual (toName n) (toQName ns)

-- ## Paths

namePath
  :: Name
  -> String
namePath (Name ps)
  = mconcat (show <$> ps)

-- | Convert a module name to a 'FilePath'.
qNamePath
  :: QName
  -> FilePath
qNamePath (QName n)
  = namePath n <.> "agda"
qNamePath (Qual n ns)
  = namePath n </> qNamePath ns

-- | Convert a 'FilePath' to a module name.
pathQName
  :: FilePath
  -- ^ The project root directory.
  -> FilePath
  -- ^ The path to the module.
  -> Maybe QName
pathQName p p'
  = List.stripPrefix (splitDirectories p) (splitDirectories p')
  >>= pathQNameRelative

pathQNameRelative
  :: [String]
  -> Maybe QName
pathQNameRelative []
  = Nothing
pathQNameRelative [n]
  = QName <$> pathName n
pathQNameRelative (n : ns@(_ : _))
  = Qual (Name [Id n]) <$> pathQNameRelative ns

pathName
  :: String
  -> Maybe Name
pathName n
  = Name . (: []) . Id <$> stripSuffix ".agda" n

-- ## Match

-- | Given a string of tokens found in a raw application, filter the given list
-- of names by whether each name's identifiers appear in order.
matchOperators
  :: [String]
  -- ^ A string of tokens found in a raw application.
  -> [Name]
  -- ^ A list of names to consider.
  -> [Name]
matchOperators ss
  = filter (\n -> isOperator n && isSubsequenceOf (nameIds n) ss)

