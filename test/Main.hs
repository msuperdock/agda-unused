module Main where

import Agda.Unused
  (Unused(..), UnusedItems(..))
import Agda.Unused.Check
  (checkUnused, checkUnusedLocal)
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

import Paths_agda_unused
  (getDataFileName)

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

-- ## Types

data TestMode where

  Global
    :: TestMode

  Local
    :: TestMode

  deriving Show

testMode
  :: a
  -> a
  -> TestMode
  -> a
testMode x _ Global
  = x
testMode _ x Local
  = x

data TestType where

  Pattern
    :: TestType

  Expression
    :: TestType

  Declaration
    :: TestType

  deriving Show

testTypeDir
  :: TestType
  -> FilePath
testTypeDir Pattern
  = "pattern"
testTypeDir Expression
  = "expression"
testTypeDir Declaration
  = "declaration"

testTypePath
  :: TestType
  -> IO FilePath
testTypePath t
  = getDataFileName ("data/test" </> testTypeDir t)

-- ## Expectations

testCheck
  :: TestMode
  -- ^ Mode of test.
  -> TestType
  -- ^ Type of test.
  -> String
  -- ^ Name of test module.
  -> [String]
  -- ^ Expected unused identifiers.
  -> Expectation
testCheck m t m' us
  = testCheckNames m t m' (name <$> us)

testCheckNames
  :: TestMode
  -- ^ Mode of test.
  -> TestType
  -- ^ Type of test.
  -> String
  -- ^ Name of test module.
  -> [QName]
  -- ^ Expected unused names.
  -> Expectation
testCheckNames m t m' us
  = testTypePath t
  >>= \p -> testMode
    (fmap unusedItems <$> checkUnused p [Root (name m') (Just [])])
    (checkUnusedLocal p (name m')) m
  >>= testUnused (Set.fromList us)

testUnused
  :: Set QName
  -> Either Error UnusedItems
  -> Expectation
testUnused _ (Left _)
  = expectationFailure ""
testUnused ns (Right (UnusedItems rs))
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
    (testCheck Global Pattern "IdentP" ["y", "f", "g"])
  >> it "checks operator applications (OpAppP)"
    (testCheckNames Global Pattern "OpAppP" [land])
  >> it "checks as-patterns (AsP)"
    (testCheck Global Pattern "AsP" ["y", "z", "w", "z'", "w'", "f", "g"])

testExpression
  :: Spec
testExpression
  = describe "expressions"
  $ it "checks with-applications (WithApp)"
    (testCheck Global Expression "WithApp" ["f", "g"])
  >> it "checks lambdas (Lam)"
    (testCheck Global Expression "Lam" ["y", "y'", "f", "g"])
  >> it "checks extended lambdas (ExtendedLam)"
    (testCheck Global Expression "ExtendedLam" ["x", "f"])
  >> it "checks pi-types (Pi)"
    (testCheck Global Expression "Pi" ["y", "w", "f"])
  >> it "checks let-blocks (Let)"
    (testCheck Global Expression "Let" ["z", "f"])
  >> it "checks do-blocks (DoBlock)"
    (testCheck Global Expression "DoBlock1" ["z", "f"]
    >> testCheckNames Global Expression "DoBlock2" [bind_, name "f"]
    >> testCheckNames Global Expression "DoBlock3" [bind, name "f"]
    >> testCheckNames Global Expression "DoBlock4" [bind, bind_, name "f"])

testDeclaration
  :: Spec
testDeclaration
  = describe "declarations"
  $ it "checks type signatures (TypeSig)"
    (testCheck Global Declaration "TypeSig" ["g", "h"])
  >> it "checks function clauses (FunClause)"
    (testCheck Global Declaration "FunClause" ["z", "f", "snoc"])
  >> it "checks data signatures (DataSig)"
    (testCheck Global Declaration "DataSig" ["D"])
  >> it "checks record definitions (RecordDef)"
    (testCheck Global Declaration "RecordDef" ["x"])
  >> it "checks syntax declarations (Syntax)"
    (testCheck Global Declaration "Syntax" ["p1", "p1'"])
  >> it "checks pattern synonyms (PatternSyn)"
    (testCheck Global Declaration "PatternSyn" ["q", "f", "g"])
  >> it "checks macros (Macro)"
    (testCheck Global Declaration "Macro" ["B", "C", "O"]
    >> testCheck Local Declaration "Macro" [])
  >> it "checks postulates (Postulate)"
    (testCheck Global Declaration "Postulate" ["g", "h"])
  >> it "checks open statements (Open)"
    (testCheck Global Declaration "Open" ["N", "O", "x", "y", "z"])

-- ## Main

main
  :: IO ()
main
  = hspec testAll

