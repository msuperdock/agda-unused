module Agda.Unused.Name
  ( Name (..)
  , NamePart (..)
  , QName (..)
  , fromAsName
  , fromName
  , fromNameRange
  , fromQName
  , fromQNameRange
  , isBuiltin
  , isOperator
  , matchNames
  , nameIds
  , qNamePath
  , stripPrefix
  ) where

import Agda.Syntax.Concrete
  (AsName, AsName' (..))
import Agda.Syntax.Concrete.Name
  (NamePart (..))
import qualified Agda.Syntax.Concrete.Name
  as N
import Agda.Syntax.Position
  (Range)
import Data.List
  (isSubsequenceOf)
import Data.Maybe
  (mapMaybe)
import System.FilePath
  ((</>), (<.>))

-- ## Definitions

newtype Name
  = Name
  { nameParts
    :: [NamePart]
  } deriving (Eq, Ord, Show)

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

nameIds
  :: Name
  -> [String]
nameIds (Name ps)
  = mapMaybe namePartId ps

isOperator
  :: Name
  -> Bool
isOperator (Name [])
  = False
isOperator (Name (_ : []))
  = False
isOperator (Name (_ : _ : _))
  = True

isBuiltin
  :: QName
  -> Bool
isBuiltin (Qual (Name [Id "Agda"]) _)
  = True
isBuiltin _
  = False

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

fromName
  :: N.Name
  -> Maybe Name
fromName (N.NoName _ _)
  = Nothing
fromName (N.Name _ _ n)
  = Just (Name n)

fromNameRange
  :: N.Name
  -> Maybe (Range, Name)
fromNameRange (N.NoName _ _)
  = Nothing
fromNameRange (N.Name r _ n)
  = Just (r, Name n)

fromQName
  :: N.QName
  -> Maybe QName
fromQName (N.QName n)
  = QName <$> fromName n
fromQName (N.Qual n ns)
  = Qual <$> fromName n <*> fromQName ns

fromQNameRange
  :: N.QName
  -> Maybe (Range, QName)
fromQNameRange (N.QName n)
  = fmap QName <$> fromNameRange n
fromQNameRange (N.Qual n ns)
  = fmap . Qual <$> fromName n <*> fromQNameRange ns

fromAsName
  :: AsName
  -> Maybe Name
fromAsName (AsName (Left _) _)
  = Nothing
fromAsName (AsName (Right n) _)
  = fromName n

namePartId
  :: NamePart
  -> Maybe String
namePartId Hole
  = Nothing
namePartId (Id s)
  = Just s

-- ## Paths

namePath
  :: Name
  -> String
namePath (Name ps)
  = mconcat (show <$> ps)

qNamePath
  :: QName
  -> FilePath
qNamePath (QName n)
  = namePath n <.> "agda"
qNamePath (Qual n ns)
  = namePath n </> qNamePath ns

-- ## Match

matchName
  :: [String]
  -> Name
  -> Bool
matchName ss n
  = isSubsequenceOf (nameIds n) ss

matchNames
  :: [String]
  -> [Name]
  -> [Name]
matchNames ss
  = filter (matchName ss)

