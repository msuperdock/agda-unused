module Main where

import Agda.Unused
  (UnusedItems(..), UnusedOptions(..))
import Agda.Unused.Check
  (checkUnused, checkUnusedWith)
import Agda.Unused.Monad.Error
  (Error)
import Agda.Unused.Monad.Reader
  (Mode(..))
import Agda.Unused.Print
  (printError, printUnusedItems)
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
import Data.List.NonEmpty
  (NonEmpty(..))
import qualified Data.Set
  as Set
import Data.Text
  (Text)
import qualified Data.Text
  as T
import System.FilePath
  ((</>), (<.>))
import Test.Hspec
  (Expectation, Spec, describe, expectationFailure, hspec, it, shouldBe,
    shouldSatisfy)

import Paths_agda_unused
  (getDataFileName)

-- ## Names

name0
  :: String
  -> Name
name0 n
  = Name (Id n :| [])

name2
  :: String
  -> Name
name2 n
  = Name (Hole :| Id n : Hole : [])

name
  :: String
  -> QName
name n
  = QName (name0 n)
 
land
  :: QName
land
  = QName (name2 "&&")

bind
  :: QName
bind
  = QName (name2 ">>=")

bind_
  :: QName
bind_
  = QName (name2 ">>")

agdaBuiltinBool
  :: QName
agdaBuiltinBool
  = Qual (name0 "Agda")
  $ Qual (name0 "Builtin")
  $ QName (name0 "Bool")

agdaBuiltinNat
  :: QName
agdaBuiltinNat
  = Qual (name0 "Agda")
  $ Qual (name0 "Builtin")
  $ QName (name0 "Nat")

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
testCheck t = do
  rootPath
    <- testRootPath t
  filePath
    <- testFilePath t
  unusedLocal
    <- checkUnusedWith Local (unusedOptions rootPath) filePath
  unusedGlobal
    <- checkUnusedWith Global (unusedOptions rootPath) filePath
  _
    <- testUnused unusedLocal (mapMaybe privateMay (testResult t))
  _
    <- testUnused unusedGlobal (snd <$> testResult t)
  pure ()

testCheckExample
  :: Expectation
testCheckExample = do
  rootPath
    <- getDataFileName "data/example"
  filePath
    <- getDataFileName "data/example/Test.agda"
  unused
    <- checkUnused (unusedOptions rootPath) filePath
  _
    <- testUnusedExample unused
  pure ()

testUnused
  :: Either Error UnusedItems
  -> [RangeInfo]
  -> Expectation
testUnused (Left e) _
  = expectationFailure (T.unpack (printError e))
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
  = (t0 `shouldSatisfy` T.isSuffixOf "/Test.agda:4.23-27")
  >> (t1 `shouldBe` "  unused imported item ‘true’")
  >> (t2 `shouldSatisfy` T.isSuffixOf "/Test.agda:5.1-30")
  >> (t3 `shouldBe` "  unused import ‘Agda.Builtin.Unit’")
  >> (t4 `shouldSatisfy` T.isSuffixOf "/Test.agda:11.9-10")
  >> (t5 `shouldBe` "  unused variable ‘x’")
testUnusedOutput _
  = expectationFailure ""

unusedOptions
  :: FilePath
  -> UnusedOptions
unusedOptions p
  = UnusedOptions
  { unusedOptionsInclude
    = [p]
  , unusedOptionsLibraries
    = []
  , unusedOptionsLibrariesFile
    = Nothing
  , unusedOptionsUseLibraries
    = False
  , unusedOptionsUseDefaultLibraries
    = False
  }

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

  Integration
    :: !IntegrationTest
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

  RawApp
    :: ExpressionTest

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

  AnyWhere
    :: DeclarationTest

  LeftLet'
    :: DeclarationTest

  NiceFunClause
    :: DeclarationTest

  RecordModuleInstance
    :: DeclarationTest

  InstanceDef
    :: DeclarationTest

  deriving Show

data IntegrationTest where

  CrossFile
    :: IntegrationTest

  SuppressImport
    :: IntegrationTest

  SuppressDefinition
    :: IntegrationTest

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
testDir (Integration _)
  = "integration"

testRootPath
  :: Test
  -> IO FilePath
testRootPath t
  = getDataFileName ("data" </> testDir t)

testFilePath
  :: Test
  -> IO FilePath
testFilePath t
  = getDataFileName ("data" </> testDir t </> testFileName t <.> "agda")

testFileName
  :: Test
  -> String
testFileName (Pattern IdentP)
  = "IdentP"
testFileName (Pattern OpAppP)
  = "OpAppP"
testFileName (Pattern AsP)
  = "AsP"
testFileName (Expression RawApp)
  = "RawApp"
testFileName (Expression WithApp)
  = "WithApp"
testFileName (Expression Lam)
  = "Lam"
testFileName (Expression ExtendedLam)
  = "ExtendedLam"
testFileName (Expression Pi)
  = "Pi"
testFileName (Expression Let)
  = "Let"
testFileName (Expression DoBlock1)
  = "DoBlock1"
testFileName (Expression DoBlock2)
  = "DoBlock2"
testFileName (Expression DoBlock3)
  = "DoBlock3"
testFileName (Expression DoBlock4)
  = "DoBlock4"
testFileName (Declaration TypeSig)
  = "TypeSig"
testFileName (Declaration FunClause)
  = "FunClause"
testFileName (Declaration Data')
  = "Data"
testFileName (Declaration DataDef)
  = "DataDef"
testFileName (Declaration Record')
  = "Record"
testFileName (Declaration RecordDef)
  = "RecordDef"
testFileName (Declaration Syntax)
  = "Syntax"
testFileName (Declaration PatternSyn)
  = "PatternSyn"
testFileName (Declaration Mutual1)
  = "Mutual1"
testFileName (Declaration Mutual2)
  = "Mutual2"
testFileName (Declaration Abstract)
  = "Abstract"
testFileName (Declaration Private')
  = "Private"
testFileName (Declaration Postulate')
  = "Postulate"
testFileName (Declaration Open1)
  = "Open1"
testFileName (Declaration Open2)
  = "Open2"
testFileName (Declaration Import')
  = "Import"
testFileName (Declaration ModuleMacro)
  = "ModuleMacro"
testFileName (Declaration Module')
  = "Module"
testFileName (Declaration AnyWhere)
  = "AnyWhere"
testFileName (Declaration LeftLet')
  = "LeftLet"
testFileName (Declaration NiceFunClause)
  = "NiceFunClause"
testFileName (Declaration RecordModuleInstance)
  = "RecordModuleInstance"
testFileName (Declaration InstanceDef)
  = "InstanceDef"
testFileName (Integration CrossFile)
  = "CrossFile"
testFileName (Integration SuppressImport)
  = "SuppressImport"
testFileName (Integration SuppressDefinition)
  = "SuppressDefinition"

testResult
  :: Test
  -> [(Access, RangeInfo)]
testResult t
  = case t of

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
    , private (name "z'")
      ~: Variable
    , private (name "x'''")
      ~: Variable
    , private (name "z'''")
      ~: Variable
    , private (name "x''''")
      ~: Variable
    , public (name "f")
      ~: Definition
    , public (name "g")
      ~: Definition
    , public (name "h")
      ~: Definition
    , public (name "i")
      ~: Definition
    , public (name "j")
      ~: Definition
    , private (name "l")
      ~: Definition
    , public (name "m")
      ~: Definition
      -- let open (NiceOpen path): p uses plain-val; q's open is unused
      -- Without the NiceOpen fix, p and q cause "Internal error: Invalid let statement"
    , public (name "p")
      ~: Definition
    , public (name "q")
      ~: Definition
    , private (name "Plain")
      ~: Open
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

  Expression RawApp ->
    [ public (name "f")
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
    , public (name "F")
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
    , private agdaBuiltinNat
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

  Declaration AnyWhere ->
    [ public (name "swap")
      ~: Definition
    , public (name "g")
      ~: Definition
    ]

  Declaration LeftLet' ->
    [ private (name "b")
      ~: Variable
    , public (name "f")
      ~: Definition
    ]

  Declaration NiceFunClause ->
    [ private (name "b")
      ~: Variable
    , public (name "f")
      ~: Definition
    ]

  -- SectionApp via explicit arg: B unused in anonymous open; M.A used.
  -- RecordModuleInstance via ⦃ ... ⦄: N.B used; two anonymous opens
  --   (one entirely unused, one with A' unused but B' used).
  Declaration RecordModuleInstance ->
    [ private (name "B")    -- SectionApp open: B not referenced
      ~: OpenItem
    , public (name "a")     -- postulate using opened A
      ~: Postulate
    , public (name "b")     -- postulate using M.A
      ~: Postulate
    , public (name "c")     -- postulate using N.B
      ~: Postulate
    , private (name "R")    -- RecordModuleInstance open: entirely unused
      ~: Open
    , private (name "A")    -- RecordModuleInstance open: A not referenced
      ~: OpenItem
    , private (name "A'")   -- RecordModuleInstance open: A' not referenced (B' used)
      ~: OpenItem
    , public (name "d")     -- postulate using opened B'
      ~: Postulate
    ]

  Declaration InstanceDef ->
    [ private (name "g")
      ~: Postulate
    , public (name "h")
      ~: Definition
    ]

  -- Regression: CrossFileDep's 'true' (bytes 70–74) falls inside
  -- CrossFile's 'open import Agda.Builtin.Bool' (bytes 45–74).
  Integration CrossFile ->
    [ private (name "CrossFileDep")
      ~: Import
    , private agdaBuiltinBool
      ~: Import
    , private (name "true")
      ~: ImportItem
    , public (name "x")
      ~: Definition
    ]

  Integration SuppressImport ->
    [ private (name "A")
      ~: ImportItem
    , public (name "g")
      ~: Definition
    , public (name "f")
      ~: Definition
    ]

  Integration SuppressDefinition ->
    [ private (name "unused-regex")
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
  >> testIntegration
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
  $ it "checks operator sections (RawApp)"
    (testCheck (Expression RawApp))
  >> it "checks with-applications (WithApp)"
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
  >> it "checks where-open in with-clauses (AnyWhere)"
    (testCheck (Declaration AnyWhere))
  >> it "checks left-hand side let (LeftLet)"
    (testCheck (Declaration LeftLet'))
  >> it "checks let-patterns in type signatures (NiceFunClause)"
    (testCheck (Declaration NiceFunClause))
  >> it "checks record module instances (RecordModuleInstance)"
    (testCheck (Declaration RecordModuleInstance))
  >> it "checks instance declarations (InstanceDef)"
    (testCheck (Declaration InstanceDef))

testIntegration
  :: Spec
testIntegration
  = describe "integration"
  $ it "distinguishes files"
    (testCheck (Integration CrossFile))
  >> it "checks suppression comments on imports (SuppressImport)"
    (testCheck (Integration SuppressImport))
  >> it "checks suppression comments on definitions (SuppressDefinition)"
    (testCheck (Integration SuppressDefinition))

testExample
  :: Spec
testExample
  = describe "example"
  $ it "outputs the text in README.md"
  $ testCheckExample

