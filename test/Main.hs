module Main where

import Agda.Unused
  (checkUnused)
import Agda.Unused.Types.Name
  (Name(..), NamePart(..), QName(..))
import Agda.Unused.Types.Range
  (Range, RangeInfo, rangeName)
import Agda.Unused.Types.Root
  (Root(..))

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
  >> it "checks variables bound in do-blocks"
    (used "Do" "y" >> unused "Do" "z")
  >> it "checks variables bound in let-blocks"
    (used "Let" "y" >> unused "Let" "z")
  >> it "checks variables bound in where-blocks"
    (used "Where" "y" >> unused "Where" "z")
  >> it "checks variables in as-patterns"
    (used "As" "x" >> unused "As" "y" >> unused "As" "z" >> unused "As" "w")
  >> it "checks definitions"
    (used "Definition" "f" >> unused "Definition" "g")
  >> it "checks postulates"
    (used "Postulate" "f" >> unused "Postulate" "g")

  >> it "handles variables in with-clauses"
    (used "With" "x")
  >> it "handles variables in multiple with-clauses"
    (used "With2" "y")
  >> it "handles variables in as-patterns followed by with-clauses"
    (used "As2" "x" >> unused "As2" "z" >> unused "As2" "w")
  >> it "handles record constructors in modules"
    (used "Record" "c")
  >> it "handles syntax declarations"
    (used "Syntax" "S")
  >> it "handles imported syntax declarations"
    (used "Syntax2" "S")

  >> it "ignores variables in lone data signatures"
    (used "Data" "A")
  >> it "ignores constructors in function definitions"
    (used "Constructor" "false" >> used "Constructor" "true")
  >> it "ignores constructors in pattern-matching lambdas"
    (used "Lambda3" "false")
  >> it "ignores patterns in function definitions"
    (used "Pattern" "p" >> used "Pattern2" ",")
  >> it "ignores unnamed definitions"
    (used "Underscore" "_")
  >> it "ignores recursive calls to the function being defined"
    (unused "Recursive" "snoc")

