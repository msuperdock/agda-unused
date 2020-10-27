{- |
Module: Agda.Unused.Parse

Parsers for roots.
-}
module Agda.Unused.Parse
  ( parseConfig
  ) where

import Agda.Unused.Types.Name
  (Name(..), NamePart(..), QName(..))
import Agda.Unused.Types.Root
  (Root(..), Roots, fromList)
import Agda.Unused.Utils
  (mapLeft)

import Control.Monad
  (void)
import Data.Char
  (isSpace)
import Data.Text
  (Text)
import qualified Data.Text
  as T
import Data.Void
  (Void)
import Text.Megaparsec
  (Parsec, (<|>), between, many, parse, satisfy, some, try)
import Text.Megaparsec.Char
  (char, space1)
import qualified Text.Megaparsec.Char.Lexer
  as L
import Text.Megaparsec.Error
  (errorBundlePretty)

-- ## Utilities

isNameChar
  :: Char
  -> Bool
isNameChar '.'
  = False
isNameChar '('
  = False
isNameChar ')'
  = False
isNameChar c | isSpace c
  = False
isNameChar _
  = True

listMaybe
  :: [a]
  -> Maybe [a]
listMaybe []
  = Nothing
listMaybe xs@(_ : _)
  = Just xs

-- ## Parsers

type Parser
  = Parsec Void Text

parseSpace
  :: Parser ()
parseSpace
  = L.space space1
    (L.skipLineComment "--")
    (L.skipBlockComment "{-" "-}")

parseDot
  :: Parser ()
parseDot
  = void (char '.')

parseHyphen
  :: Parser ()
parseHyphen
  = void (char '-')

parseParens
  :: Parser a
  -> Parser a
parseParens
  = between (char '(') (char ')')

parseNamePart
  :: Parser NamePart
parseNamePart
  = Id <$> some (satisfy isNameChar)

parseName
  :: Parser Name
parseName
  = Name <$> some parseNamePart

parseQName
  :: Parser QName
parseQName
  = try (Qual <$> parseName <* parseDot <*> parseQName)
  <|> QName <$> parseName

parseIgnore
  :: Parser QName
parseIgnore
  = parseParens (between parseSpace parseSpace parseQName)
  <* parseSpace

parseRootName
  :: Parser QName
parseRootName
  = parseHyphen
  *> parseSpace
  *> parseQName
  <* parseSpace

parseRootNames
  :: Parser (Maybe [QName])
parseRootNames
  = listMaybe <$> many parseRootName

parseRoot
  :: Parser Root
parseRoot
  = Root
  <$> parseQName
  <* parseSpace
  <*> parseRootNames

parseRootEither
  :: Parser (Either QName Root)
parseRootEither
  = Left <$> parseIgnore
  <|> Right <$> parseRoot

parseRoots
  :: Parser Roots
parseRoots
  = fromList <$> many parseRootEither

-- | Parse configuration, producing either an error or a collection of roots.
parseConfig
  :: Text
  -> Either Text Roots
parseConfig
  = mapLeft T.pack
  . mapLeft errorBundlePretty
  . parse (parseSpace *> parseRoots) ".agda-roots"

