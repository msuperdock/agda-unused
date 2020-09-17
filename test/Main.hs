module Main where

import Agda.Unused
  (Unused(..), UnusedItems(..))
import Agda.Unused.Check
  (checkUnused)
import Agda.Unused.Monad.Error
  (Error)
import Agda.Unused.Types.Name
  (Name(..), NamePart(..), QName(..))
import Agda.Unused.Types.Range
  (rangeName)
import Agda.Unused.Types.Root
  (Root(..))

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

land
  :: QName
land
  = QName
  $ Name
  [ Hole
  , Id "&&"
  , Hole
  ]

bind
  :: QName
bind
  = QName
  $ Name
  [ Hole
  , Id ">>="
  , Hole
  ]

bind_
  :: QName
bind_
  = QName
  $ Name
  [ Hole
  , Id ">>"
  , Hole
  ]

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
  = testCheckNames f m (name <$> us)

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

testCheckNames
  :: String
  -- ^ Folder of test file.
  -> String
  -- ^ Name of test module.
  -> [QName]
  -- ^ Expected unused names.
  -> Expectation
testCheckNames f m us
  = checkUnused ("data/test" </> f) [Root (name m) []]
  >>= testUnused (Set.fromList us)

testCheckNamesPattern
  :: String
  -- ^ Name of test module.
  -> [QName]
  -- ^ Expected unused names.
  -> Expectation
testCheckNamesPattern
  = testCheckNames "pattern"

testCheckNamesExpression
  :: String
  -- ^ Name of test module.
  -> [QName]
  -- ^ Expected unused names.
  -> Expectation
testCheckNamesExpression
  = testCheckNames "expression"

testUnused
  :: Set QName
  -> Either Error Unused
  -> Expectation
testUnused _ (Left _)
  = expectationFailure ""
testUnused ns (Right (Unused (UnusedItems rs) _))
  = Set.fromList (rangeName <$> Map.elems rs) `shouldBe` ns

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
  >> it "checks operator applications (OpAppP)"
    (testCheckNamesPattern "OpAppP" [land])
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
    (testCheckExpression "DoBlock1" ["z", "f"]
    >> testCheckNamesExpression "DoBlock2" [bind_, name "f"]
    >> testCheckNamesExpression "DoBlock3" [bind, name "f"]
    >> testCheckNamesExpression "DoBlock4" [bind, bind_, name "f"])

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
  >> it "checks macros (Macro)"
    (testCheckDeclaration "Macro" ["B", "C", "O"])
  >> it "checks postulates (Postulate)"
    (testCheckDeclaration "Postulate" ["g", "h"])
  >> it "checks open statements (Open)"
    (testCheckDeclaration "Open" ["N", "O", "x", "y", "z"])

-- ## Main

main
  :: IO ()
main
  = hspec testAll

