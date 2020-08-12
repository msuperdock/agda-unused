{- |
Module: Agda.Unused.Types.Config

Parsing function for configuration file, which contains a list of roots.
-}
module Agda.Unused.Config
  ( parseConfig
  ) where

import Agda.Unused.Types.Name
  (Name(..), NamePart(..), QName(..))
import Agda.Unused.Types.Root
  (Root(..))
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
  (Parsec, (<|>), many, parse, satisfy, some, try)
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
isNameChar c | isSpace c
  = False
isNameChar _
  = True

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

parseNamePart
  :: Parser NamePart
parseNamePart
  = Id
    <$> some (satisfy isNameChar)

parseName
  :: Parser Name
parseName
  = Name
    <$> some parseNamePart

parseQName
  :: Parser QName
parseQName
  = try (Qual
    <$> parseName
    <*  parseDot
    <*> parseQName)
  <|> QName
    <$> parseName

-- Consume spaces afterwards.
parseRootName
  :: Parser QName
parseRootName
  = parseHyphen
   *> parseSpace
   *> parseQName
  <*  parseSpace

-- Consume spaces afterwards.
parseRoot
  :: Parser Root
parseRoot
  = Root
    <$> parseQName
    <*  parseSpace
    <*> many parseRootName

-- | Parse configuration, producing either an error message or a list of roots.
parseConfig
  :: Text
  -> Either Text [Root]
parseConfig
  = mapLeft T.pack
  . mapLeft errorBundlePretty
  . parse (many parseRoot) ".agda-roots"

