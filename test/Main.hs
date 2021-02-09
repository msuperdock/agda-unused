module Main where

import Agda.Unused
  (Unused(..), UnusedItems(..))
import Agda.Unused.Check
  (checkUnused, checkUnusedGlobal)
import Agda.Unused.Monad.Error
  (Error)
import Agda.Unused.Print
  (printUnusedItems)
import Agda.Unused.Types.Access
  (Access(..))
import Agda.Unused.Types.Name
  (Name(..), NamePart(..), QName(..))
import Agda.Unused.Types.Range
  (RangeInfo(..))
import qualified Agda.Unused.Types.Range
  as R

import Data.Maybe
  (mapMaybe)
import qualified Data.Set
  as Set
import Data.Text
  (Text)
import qualified Data.Text
  as T
import System.FilePath
  ((</>))
import Test.Hspec
  (Expectation, Spec, describe, expectationFailure, hspec, it, shouldBe,
    shouldSatisfy)

import Paths_agda_unused
  (getDataFileName)

-- ## Names

name
  :: String
  -> QName
name n
  = QName (Name [Id n])

land
  :: QName
land
  = QName (Name [Hole, Id "&&", Hole])

bind
  :: QName
bind
  = QName (Name [Hole, Id ">>=", Hole])

bind_
  :: QName
bind_
  = QName (Name [Hole, Id ">>", Hole])

agdaBuiltinBool
  :: QName
agdaBuiltinBool
  = Qual (Name [Id "Agda"])
  $ Qual (Name [Id "Builtin"])
  $ QName (Name [Id "Bool"])

-- ## Ranges

data RangeType where

  Data
    :: RangeType

  Definition
    :: RangeType

  Import
    :: RangeType

  ImportItem
    :: RangeType

  Module
    :: RangeType

  ModuleItem
    :: RangeType

  Mutual
    :: RangeType

  Open
    :: RangeType

  OpenItem
    :: RangeType

  PatternSynonym
    :: RangeType

  Postulate
    :: RangeType

  Record
    :: RangeType

  RecordConstructor
    :: RangeType

  Variable
    :: RangeType

  deriving Show

rangeInfo
  :: QName
  -> RangeType
  -> RangeInfo
rangeInfo n Data
  = RangeNamed R.RangeData n
rangeInfo n Definition
  = RangeNamed R.RangeDefinition n
rangeInfo n Import
  = RangeNamed R.RangeImport n
rangeInfo n ImportItem
  = RangeNamed R.RangeImportItem n
rangeInfo n Module
  = RangeNamed R.RangeModule n
rangeInfo n ModuleItem
  = RangeNamed R.RangeModuleItem n
rangeInfo _ Mutual
  = RangeMutual
rangeInfo n Open
  = RangeNamed R.RangeOpen n
rangeInfo n OpenItem
  = RangeNamed R.RangeOpenItem n
rangeInfo n PatternSynonym
  = RangeNamed R.RangePatternSynonym n
rangeInfo n Postulate
  = RangeNamed R.RangePostulate n
rangeInfo n Record
  = RangeNamed R.RangeRecord n
rangeInfo n RecordConstructor
  = RangeNamed R.RangeRecordConstructor n
rangeInfo n Variable
  = RangeNamed R.RangeVariable n

private
  :: QName
  -> (Access, QName)
private n
  = (Private, n)

public
  :: QName
  -> (Access, QName)
public n
  = (Public, n)

(~:)
  :: (Access, QName)
  -> RangeType
  -> (Access, RangeInfo)
(a, n) ~: t
  = (a, rangeInfo n t)

-- ## Expectations

testCheck
  :: Test
  -> Expectation
testCheck n = do
  path
    <- testPath n
  module'
    <- pure (name (testModule n))
  unused
    <- checkUnused path module'
  unusedGlobal
    <- checkUnusedGlobal True path module'
  _
    <- testUnused unused (mapMaybe privateMay (testResult n))
  _
    <- testUnused (unusedItems <$> unusedGlobal) (snd <$> testResult n)
  pure ()

testCheckExample
  :: Expectation
testCheckExample = do
  path
    <- getDataFileName "data/test/example"
  unused
    <- checkUnused path (name "Test")
  _
    <- testUnusedExample unused
  pure ()

testUnused
  :: Either Error UnusedItems
  -> [RangeInfo]
  -> Expectation
testUnused (Left _) _
  = expectationFailure ""
testUnused (Right (UnusedItems is)) rs
  = Set.fromList (snd <$> is) `shouldBe` Set.fromList rs

testUnusedExample
  :: Either Error UnusedItems
  -> Expectation
testUnusedExample (Left _)
  = expectationFailure ""
testUnusedExample (Right is)
  = testUnusedOutput (T.lines <$> printUnusedItems is)

testUnusedOutput
  :: Maybe [Text]
  -> Expectation
testUnusedOutput (Just [t0, t1, t2, t3, t4, t5])
  = (t0 `shouldSatisfy` T.isSuffixOf "/Test.agda:4,23-27")
  >> (t1 `shouldBe` "  unused imported item ‘true’")
  >> (t2 `shouldSatisfy` T.isSuffixOf "/Test.agda:5,1-30")
  >> (t3 `shouldBe` "  unused import ‘Agda.Builtin.Unit’")
  >> (t4 `shouldSatisfy` T.isSuffixOf "/Test.agda:11,9-10")
  >> (t5 `shouldBe` "  unused variable ‘x’")
testUnusedOutput _
  = expectationFailure ""

privateMay
  :: (Access, a)
  -> Maybe a
privateMay (Private, x)
  = Just x
privateMay (Public, _)
  = Nothing

-- ## Tests

data Test where

  Pattern
    :: !PatternTest
    -> Test

  Expression
    :: !ExpressionTest
    -> Test

  Declaration
    :: !DeclarationTest
    -> Test

  deriving Show

data PatternTest where

  IdentP
    :: PatternTest 

  OpAppP
    :: PatternTest

  AsP
    :: PatternTest

  deriving Show

data ExpressionTest where

  WithApp
    :: ExpressionTest

  Lam
    :: ExpressionTest

  ExtendedLam
    :: ExpressionTest

  Pi
    :: ExpressionTest

  Let
    :: ExpressionTest

  DoBlock1
    :: ExpressionTest

  DoBlock2
    :: ExpressionTest

  DoBlock3
    :: ExpressionTest

  DoBlock4
    :: ExpressionTest

  deriving Show

data DeclarationTest where

  TypeSig
    :: DeclarationTest

  FunClause
    :: DeclarationTest

  Data'
    :: DeclarationTest

  DataDef
    :: DeclarationTest

  Record'
    :: DeclarationTest

  RecordDef
    :: DeclarationTest

  Syntax
    :: DeclarationTest

  PatternSyn
    :: DeclarationTest

  Mutual1
    :: DeclarationTest

  Mutual2
    :: DeclarationTest

  Abstract
    :: DeclarationTest

  Private'
    :: DeclarationTest

  Postulate'
    :: DeclarationTest

  Open1
    :: DeclarationTest

  Open2
    :: DeclarationTest

  Import'
    :: DeclarationTest

  ModuleMacro
    :: DeclarationTest

  Module'
    :: DeclarationTest

  deriving Show

testDir
  :: Test
  -> FilePath
testDir (Pattern _)
  = "pattern"
testDir (Expression _)
  = "expression"
testDir (Declaration _)
  = "declaration"

testPath
  :: Test
  -> IO FilePath
testPath n
  = getDataFileName ("data/test" </> testDir n)

testModule
  :: Test
  -> String
testModule (Pattern IdentP)
  = "IdentP"
testModule (Pattern OpAppP)
  = "OpAppP"
testModule (Pattern AsP)
  = "AsP"
testModule (Expression WithApp)
  = "WithApp"
testModule (Expression Lam)
  = "Lam"
testModule (Expression ExtendedLam)
  = "ExtendedLam"
testModule (Expression Pi)
  = "Pi"
testModule (Expression Let)
  = "Let"
testModule (Expression DoBlock1)
  = "DoBlock1"
testModule (Expression DoBlock2)
  = "DoBlock2"
testModule (Expression DoBlock3)
  = "DoBlock3"
testModule (Expression DoBlock4)
  = "DoBlock4"
testModule (Declaration TypeSig)
  = "TypeSig"
testModule (Declaration FunClause)
  = "FunClause"
testModule (Declaration Data')
  = "Data"
testModule (Declaration DataDef)
  = "DataDef"
testModule (Declaration Record')
  = "Record"
testModule (Declaration RecordDef)
  = "RecordDef"
testModule (Declaration Syntax)
  = "Syntax"
testModule (Declaration PatternSyn)
  = "PatternSyn"
testModule (Declaration Mutual1)
  = "Mutual1"
testModule (Declaration Mutual2)
  = "Mutual2"
testModule (Declaration Abstract)
  = "Abstract"
testModule (Declaration Private')
  = "Private"
testModule (Declaration Postulate')
  = "Postulate"
testModule (Declaration Open1)
  = "Open1"
testModule (Declaration Open2)
  = "Open2"
testModule (Declaration Import')
  = "Import"
testModule (Declaration ModuleMacro)
  = "ModuleMacro"
testModule (Declaration Module')
  = "Module"

testResult
  :: Test
  -> [(Access, RangeInfo)]
testResult n
  = case n of

  Pattern IdentP ->
    [ private (name "y")
      ~: Variable
    , public (name "f")
      ~: Definition
    , public (name "g")
      ~: Definition
    ]

  Pattern OpAppP ->
    [ public land
      ~: Definition
    ]

  Pattern AsP ->
    [ private (name "y")
      ~: Variable
    , private (name "z")
      ~: Variable
    , private (name "w")
      ~: Variable
    , private (name "z'")
      ~: Variable
    , private (name "w'")
      ~: Variable
    , public (name "f")
      ~: Definition
    , public (name "g")
      ~: Definition
    ]

  Expression WithApp ->
    [ public (name "f")
      ~: Definition
    , public (name "g")
      ~: Definition
    ]

  Expression Lam ->
    [ private (name "y")
      ~: Variable
    , private (name "y'")
      ~: Variable
    , public (name "f")
      ~: Definition
    , public (name "g")
      ~: Definition
    ]

  Expression ExtendedLam ->
    [ private (name "x")
      ~: Variable
    , public (name "f")
      ~: Definition
    ]

  Expression Pi ->
    [ private (name "y")
      ~: Variable
    , private (name "w")
      ~: Variable
    , public (name "f")
      ~: Definition
    ]

  Expression Let ->
    [ private (name "z")
      ~: Definition
    , public (name "f")
      ~: Definition
    ]

  Expression DoBlock1 ->
    [ private (name "z")
      ~: Variable
    , public (name "f")
      ~: Definition
    ]

  Expression DoBlock2 ->
    [ public bind_
      ~: Definition
    , public (name "f")
      ~: Definition
    ]

  Expression DoBlock3 ->
    [ public bind
      ~: Definition
    , public (name "f")
      ~: Definition
    ]

  Expression DoBlock4 ->
    [ public bind
      ~: Definition
    , public bind_
      ~: Definition
    , public (name "f")
      ~: Definition
    ]

  Declaration TypeSig ->
    [ public (name "g")
      ~: Definition
    , public (name "h")
      ~: Definition
    ]

  Declaration FunClause ->
    [ private (name "z")
      ~: Definition
    , public (name "f")
      ~: Definition
    , public (name "snoc")
      ~: Definition
    ]

  Declaration Data' ->
    [ public (name "E")
      ~: Data
    , public (name "_")
      ~: Mutual
    ]

  Declaration DataDef ->
    [ private (name "z")
      ~: Variable
    , public (name "d")
      ~: Postulate
    ]

  Declaration Record' ->
    [ public (name "B")
      ~: Record
    , public (name "D")
      ~: Record
    , public (name "c")
      ~: RecordConstructor
    , public (name "x")
      ~: Definition
    , public (name "y")
      ~: Definition
    ]

  Declaration RecordDef ->
    [ private (name "z")
      ~: Variable
    , public (name "r")
      ~: Postulate
    ]

  Declaration Syntax ->
    [ public (name "p1")
      ~: Postulate
    , public (name "p1'")
      ~: Postulate
    ]

  Declaration PatternSyn ->
    [ public (name "q")
      ~: PatternSynonym
    , public (name "f")
      ~: Definition
    , public (name "g")
      ~: Definition
    ]

  Declaration Mutual1 ->
    [ public (name "_")
      ~: Mutual
    ]

  Declaration Mutual2 ->
    [ public (name "is-even'")
      ~: Definition
    ]

  Declaration Abstract ->
    [ public (name "g")
      ~: Definition
    , public (name "h")
      ~: Definition
    ]

  Declaration Private' ->
    [ private (name "g")
      ~: Definition
    , private (name "h")
      ~: Definition
    ]

  Declaration Postulate' ->
    [ public (name "g")
      ~: Postulate
    , public (name "h")
      ~: Definition
    ]

  Declaration Open1 ->
    [ private (name "N")
      ~: Open
    , private (name "P")
      ~: Open
    , public (name "Q")
      ~: Module
    , private (name "x'")
      ~: OpenItem
    , public (name "v")
      ~: Definition
    , public (name "y")
      ~: Definition
    ]

  Declaration Open2 ->
    [ public (name "z")
      ~: OpenItem
    , public (name "p")
      ~: Postulate
    ]

  Declaration Import' ->
    [ private agdaBuiltinBool
      ~: Import
    , private (name "tt")
      ~: ImportItem
    , public (name "A")
      ~: Definition
    ]

  Declaration ModuleMacro ->
    [ private (name "x")
      ~: Variable
    , public (name "Q")
      ~: Module
    , public (name "A'")
      ~: ModuleItem
    , public (name "C")
      ~: Definition
    , public (name "D")
      ~: Definition
    , public (name "y")
      ~: Definition
    ]

  Declaration Module' ->
    [ public (name "O")
      ~: Module
    , public (name "P")
      ~: Module
    , public (name "x")
      ~: Definition
    ]

-- ## Main

main
  :: IO ()
main
  = hspec testAll

testAll
  :: Spec
testAll
  = describe "checkUnused"
  $ testPattern
  >> testExpression
  >> testDeclaration
  >> testExample

testPattern
  :: Spec
testPattern
  = describe "patterns"
  $ it "checks identifiers (IdentP)"
    (testCheck (Pattern IdentP))
  >> it "checks operator applications (OpAppP)"
    (testCheck (Pattern OpAppP))
  >> it "checks as-patterns (AsP)"
    (testCheck (Pattern AsP))

testExpression
  :: Spec
testExpression
  = describe "expressions"
  $ it "checks with-applications (WithApp)"
    (testCheck (Expression WithApp))
  >> it "checks lambdas (Lam)"
    (testCheck (Expression Lam))
  >> it "checks extended lambdas (ExtendedLam)"
    (testCheck (Expression ExtendedLam))
  >> it "checks pi-types (Pi)"
    (testCheck (Expression Pi))
  >> it "checks let-blocks (Let)"
    (testCheck (Expression Let))
  >> it "checks do-blocks (DoBlock)"
    (testCheck (Expression DoBlock1)
    >> testCheck (Expression DoBlock2)
    >> testCheck (Expression DoBlock3)
    >> testCheck (Expression DoBlock4))

testDeclaration
  :: Spec
testDeclaration
  = describe "declarations"
  $ it "checks type signatures (TypeSig)"
    (testCheck (Declaration TypeSig))
  >> it "checks function clauses (FunClause)"
    (testCheck (Declaration FunClause))
  >> it "checks data declarations (Data)"
    (testCheck (Declaration Data'))
  >> it "checks data definitions (DataDef)"
    (testCheck (Declaration DataDef))
  >> it "checks record declarations (Record)"
    (testCheck (Declaration Record'))
  >> it "checks record definitions (RecordDef)"
    (testCheck (Declaration RecordDef))
  >> it "checks syntax declarations (Syntax)"
    (testCheck (Declaration Syntax))
  >> it "checks pattern synonyms (PatternSyn)"
    (testCheck (Declaration PatternSyn))
  >> it "checks mutual blocks (Mutual)"
    (testCheck (Declaration Mutual1)
    >> testCheck (Declaration Mutual2))
  >> it "checks abstract blocks (Abstract)"
    (testCheck (Declaration Abstract))
  >> it "checks private blocks (Private)"
    (testCheck (Declaration Private'))
  >> it "checks postulates (Postulate)"
    (testCheck (Declaration Postulate'))
  >> it "checks open statements (Open)"
    (testCheck (Declaration Open1)
    >> testCheck (Declaration Open2))
  >> it "checks import statements (Import)"
    (testCheck (Declaration Import'))
  >> it "checks module macros (ModuleMacro)"
    (testCheck (Declaration ModuleMacro))
  >> it "checks module definitions (Module)"
    (testCheck (Declaration Module'))

testExample
  :: Spec
testExample
  = describe "example"
  $ it "outputs the text in README.md"
  $ testCheckExample

