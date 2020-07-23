module Main where

import Agda.Unused
  (checkUnused)
import Agda.Unused.Types.Name
  (Name (..), NamePart (..), QName (..))
import Agda.Unused.Types.Range
  (Range, RangeInfo, rangeName)
import Agda.Unused.Types.Root
  (Root (..))

import Data.Map.Strict
  (Map)
import qualified Data.Map.Strict
  as Map
import Test.Hspec
  (Expectation, describe, expectationFailure, hspec, it, shouldSatisfy)

hasName
  :: QName
  -> Map Range RangeInfo
  -> Bool
hasName n
  = elem n
  . fmap rangeName
  . Map.elems

used
  :: String
  -> QName
  -> Expectation
used f n
  = checkUnused "data/test" [Root (QName (Name [Id f])) []]
  >>= either (const (expectationFailure "")) (`shouldSatisfy` not . hasName n)

unused
  :: String
  -> QName
  -> Expectation
unused f n
  = checkUnused "data/test" [Root (QName (Name [Id f])) []]
  >>= either (const (expectationFailure "")) (`shouldSatisfy` hasName n)

main
  :: IO ()
main
  = hspec
  $ describe "checkUnused"
  $ it "handles variables in as-patterns"
    (used "As" (QName (Name [Id "x"])))
  >> it "handles record constructors within modules"
    (used "Record" (QName (Name [Id "c"])))
  >> it "handles variables in with-clauses"
    (used "With" (QName (Name [Id "x"])))
  >> it "handles variables in multiple with-clauses"
    (used "With2" (QName (Name [Id "y"])))
  >> it "ignores variables in lone data signatures"
    (used "Data" (QName (Name [Id "A"])))

