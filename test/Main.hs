module Main where

import Agda.Unused
  (checkUnused)
import Agda.Unused.Monad.Error
  (Error)
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
import Data.Set
  (Set)
import qualified Data.Set
  as Set
import Test.Hspec
  (Expectation, Spec, describe, expectationFailure, hspec, it, shouldBe)

-- ## Utilities

name
  :: String
  -> QName
name
  = QName
  . Name
  . (: [])
  . Id

-- ## Expectations

testCheck
  :: String
  -> [String]
  -> Expectation
testCheck m us
  = checkUnused "data/test" [Root (name m) []]
  >>= testUnused (Set.fromList (name <$> us))

testUnused
  :: Set QName
  -> Either Error (Map Range RangeInfo)
  -> Expectation
testUnused _ (Left _)
  = expectationFailure ""
testUnused us (Right rs)
  = Set.fromList (rangeName <$> Map.elems rs) `shouldBe` us

-- ## Tests

testAll
  :: Spec
testAll
  = describe "checkUnused"
  $ testPattern
  >> testExpression
  >> testDeclaration

testPattern
  :: Spec
testPattern
  = describe "patterns"
  $ it "checks identifier patterns"
    (testCheck "IdentP" ["y", "f", "g"])
  >> it "checks as-patterns"
    (testCheck "AsP" ["y", "z", "w", "z'", "w'", "f", "g"])

testExpression
  :: Spec
testExpression
  = describe "expressions"
  $ it "checks with-applications"
    (testCheck "WithApp" ["f", "g"])
  >> it "checks lambdas"
    (testCheck "Lam" ["y", "y'", "f", "g"])
  >> it "checks extended lambdas"
    (testCheck "ExtendedLam" ["x", "f"])
  >> it "checks pi-types"
    (testCheck "Pi" ["y", "w", "f"])
  >> it "checks let-blocks"
    (testCheck "Let" ["z", "f"])
  >> it "checks do-blocks"
    (testCheck "DoBlock" ["z", "f"])

testDeclaration
  :: Spec
testDeclaration
  = describe "declarations"
  $ it "checks type signatures"
    (testCheck "TypeSig" ["g", "h"])
  >> it "checks function clauses"
    (testCheck "FunClause" ["z", "f", "snoc"])
  >> it "checks data signatures"
    (testCheck "DataSig" ["D"])
  >> it "checks record definitions"
    (testCheck "RecordDef" ["x"])
  >> it "checks syntax declarations"
    (testCheck "Syntax" ["p1", "p1'"])
  >> it "checks pattern synonyms"
    (testCheck "PatternSyn" ["q", "f", "g"])
  >> it "checks mutual blocks"
    (testCheck "Mutual" ["f", "g"])
  >> it "checks postulates"
    (testCheck "Postulate" ["g", "h"])

-- ## Main

main
  :: IO ()
main
  = hspec testAll

