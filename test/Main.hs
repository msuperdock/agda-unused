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

name
  :: String
  -> QName
name
  = QName
  . Name
  . (: [])
  . Id

root
  :: String
  -> [Root]
root s
  = [Root (name s) []]

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
  -> String
  -> Expectation
used f n
  = checkUnused "data/test" (root f)
  >>= either (const (expectationFailure ""))
    (`shouldSatisfy` not . hasName (name n))

unused
  :: String
  -> String
  -> Expectation
unused f n
  = checkUnused "data/test" (root f)
  >>= either (const (expectationFailure ""))
    (`shouldSatisfy` hasName (name n))

main
  :: IO ()
main
  = hspec
  $ describe "checkUnused"

  $ it "checks variables in function definitions"
    (used "Variable" "x" >> unused "Variable" "y")
  >> it "checks variables in anonymous lambdas"
    (used "Lambda" "z" >> unused "Lambda" "y")
  >> it "checks typed variables in anonymous lambdas"
    (used "Lambda2" "z" >> unused "Lambda2" "y")
  >> it "checks variables in pattern-matching lambdas"
    (used "Lambda3" "y" >> unused "Lambda3" "x")
  >> it "checks variables bound to function arguments"
    (used "Binding" "z" >> unused "Binding" "w")
  >> it "checks variables bound to implicit function arguments"
    (used "Binding" "x" >> unused "Binding" "y")

-- checks variables bound in do-blocks.
-- checks variables bound in let-blocks.
-- checks variables bound in where-blocks.

-- checks ordinary functions.
-- checks postulates.

  -- TODO: make these follow pattern above?
  >> it "handles variables in as-patterns"
    (used "As" "x")
  >> it "handles variables in with-clauses"
    (used "With" "x")
  >> it "handles variables in multiple with-clauses"
    (used "With2" "y")

  >> it "handles record constructors in modules"
    (used "Record" "c")

  >> it "ignores variables in lone data signatures"
    (used "Data" "A")
  >> it "ignores constructors in pattern-matching lambdas"
    (used "Lambda3" "false")

-- ignores constructors in [...].
-- ignores unnamed functions.

