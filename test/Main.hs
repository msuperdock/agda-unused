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
import System.FilePath
  ((</>))
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
  -- ^ Folder of test file.
  -> String
  -- ^ Name of test module.
  -> [String]
  -- ^ Expected unused identifiers.
  -> Expectation
testCheck f m us
  = checkUnused ("data/test" </> f) [Root (name m) []]
  >>= testUnused (Set.fromList (name <$> us))

testCheckPattern
  :: String
  -- ^ Name of test module.
  -> [String]
  -- ^ Expected unused identifiers.
  -> Expectation
testCheckPattern
  = testCheck "pattern"

testCheckExpression
  :: String
  -- ^ Name of test module.
  -> [String]
  -- ^ Expected unused identifiers.
  -> Expectation
testCheckExpression
  = testCheck "expression"

testCheckDeclaration
  :: String
  -- ^ Name of test module.
  -> [String]
  -- ^ Expected unused identifiers.
  -> Expectation
testCheckDeclaration
  = testCheck "declaration"

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
  $ it "checks identifiers (IdentP)"
    (testCheckPattern "IdentP" ["y", "f", "g"])
  >> it "checks as-patterns (AsP)"
    (testCheckPattern "AsP" ["y", "z", "w", "z'", "w'", "f", "g"])

testExpression
  :: Spec
testExpression
  = describe "expressions"
  $ it "checks with-applications (WithApp)"
    (testCheckExpression "WithApp" ["f", "g"])
  >> it "checks lambdas (Lam)"
    (testCheckExpression "Lam" ["y", "y'", "f", "g"])
  >> it "checks extended lambdas (ExtendedLam)"
    (testCheckExpression "ExtendedLam" ["x", "f"])
  >> it "checks pi-types (Pi)"
    (testCheckExpression "Pi" ["y", "w", "f"])
  >> it "checks let-blocks (Let)"
    (testCheckExpression "Let" ["z", "f"])
  >> it "checks do-blocks (DoBlock)"
    (testCheckExpression "DoBlock" ["z", "f"])

testDeclaration
  :: Spec
testDeclaration
  = describe "declarations"
  $ it "checks type signatures (TypeSig)"
    (testCheckDeclaration "TypeSig" ["g", "h"])
  >> it "checks function clauses (FunClause)"
    (testCheckDeclaration "FunClause" ["z", "f", "snoc"])
  >> it "checks data signatures (DataSig)"
    (testCheckDeclaration "DataSig" ["D"])
  >> it "checks record definitions (RecordDef)"
    (testCheckDeclaration "RecordDef" ["x"])
  >> it "checks syntax declarations (Syntax)"
    (testCheckDeclaration "Syntax" ["p1", "p1'"])
  >> it "checks pattern synonyms (PatternSyn)"
    (testCheckDeclaration "PatternSyn" ["q", "f", "g"])
  >> it "checks mutual blocks (Mutual)"
    (testCheckDeclaration "Mutual" ["f", "g"])
  >> it "checks postulates (Postulate)"
    (testCheckDeclaration "Postulate" ["g", "h"])

-- ## Main

main
  :: IO ()
main
  = hspec testAll

