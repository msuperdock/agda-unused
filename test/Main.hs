module Main where

import Agda.Unused
  (Unused(..), UnusedItems(..))
import Agda.Unused.Check
  (checkUnused, checkUnusedLocal)
import Agda.Unused.Monad.Error
  (Error)
import Agda.Unused.Types.Access
  (Access(..))
import Agda.Unused.Types.Name
  (Name(..), NamePart(..), QName(..))
import Agda.Unused.Types.Range
  (RangeInfo(..))
import qualified Agda.Unused.Types.Range
  as R
import Agda.Unused.Types.Root
  (Root(..))

import Data.Maybe
  (mapMaybe)
import qualified Data.Set
  as Set
import System.FilePath
  ((</>))
import Test.Hspec
  (Expectation, Spec, describe, expectationFailure, hspec, it, shouldBe)

import Paths_agda_unused
  (getDataFileName)

-- ## Names

newtype PrivateName
  = PrivateName
  { privateQName
    :: QName
  } deriving Show

class IsName a where

  name
    :: a
    -> QName

  access
    :: a
    -> Access

instance IsName QName where

  name
    = id

  access _
    = Public

instance IsName String where

  name n
    = QName (Name [Id n])

  access _
    = Public

instance IsName PrivateName where

  name
    = privateQName

  access _
    = Private

private
  :: IsName a
  => a
  -> PrivateName
private n
  = PrivateName (name n)

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

  Record
    :: RangeType

  RecordConstructor
    :: RangeType

  Variable
    :: RangeType

  deriving Show

(~:)
  :: IsName a
  => a
  -> RangeType
  -> (Access, RangeInfo)
n ~: Data
  = (access n, RangeInfo R.RangeData (name n))
n ~: Definition
  = (access n, RangeInfo R.RangeDefinition (name n))
n ~: Import
  = (access n, RangeInfo R.RangeImport (name n))
n ~: ImportItem
  = (access n, RangeInfo R.RangeImportItem (name n))
n ~: Module
  = (access n, RangeInfo R.RangeModule (name n))
n ~: ModuleItem
  = (access n, RangeInfo R.RangeModuleItem (name n))
n ~: Mutual
  = (access n, RangeMutual)
n ~: Open
  = (access n, RangeInfo R.RangeOpen (name n))
n ~: OpenItem
  = (access n, RangeInfo R.RangeOpenItem (name n))
n ~: PatternSynonym
  = (access n, RangeInfo R.RangePatternSynonym (name n))
n ~: Record
  = (access n, RangeInfo R.RangeRecord (name n))
n ~: RecordConstructor
  = (access n, RangeInfo R.RangeRecordConstructor (name n))
n ~: Variable
  = (access n, RangeInfo R.RangeVariable (name n))

-- ## Expectations

testCheck
  :: Test
  -> Expectation
testCheck n = do
  path
    <- testPath n
  unused
    <- checkUnused path [Root (name (testModule n)) (Just [])]
  unusedLocal
    <- checkUnusedLocal path (name (testModule n))
  _
    <- testUnused (unusedItems <$> unused) (snd <$> testResult n)
  _
    <- testUnused unusedLocal (mapMaybe privateMay (testResult n))
  pure ()

testUnused
  :: Either Error UnusedItems
  -> [RangeInfo]
  -> Expectation
testUnused (Left _) _
  = expectationFailure ""
testUnused (Right (UnusedItems is)) rs
  = Set.fromList (snd <$> is) `shouldBe` Set.fromList rs

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

  DataSig
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

  Postulate
    :: DeclarationTest

  Open'
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
testModule (Declaration DataSig)
  = "DataSig"
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
testModule (Declaration Postulate)
  = "Postulate"
testModule (Declaration Open')
  = "Open"
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
    [ private "y"
      ~: Variable
    , "f"
      ~: Definition
    , "g"
      ~: Definition
    ]

  Pattern OpAppP ->
    [ land
      ~: Definition
    ]

  Pattern AsP ->
    [ private "y"
      ~: Variable
    , private "z"
      ~: Variable
    , private "w"
      ~: Variable
    , private "z'"
      ~: Variable
    , private "w'"
      ~: Variable
    , "f"
      ~: Definition
    , "g"
      ~: Definition
    ]

  Expression WithApp ->
    [ "f"
      ~: Definition
    , "g"
      ~: Definition
    ]

  Expression Lam ->
    [ private "y"
      ~: Variable
    , private "y'"
      ~: Variable
    , "f"
      ~: Definition
    , "g"
      ~: Definition
    ]

  Expression ExtendedLam ->
    [ private "x"
      ~: Variable
    , "f"
      ~: Definition
    ]

  Expression Pi ->
    [ private "y"
      ~: Variable
    , private "w"
      ~: Variable
    , "f"
      ~: Definition
    ]

  Expression Let ->
    [ private "z"
      ~: Definition
    , "f"
      ~: Definition
    ]

  Expression DoBlock1 ->
    [ private "z"
      ~: Variable
    , "f"
      ~: Definition
    ]

  Expression DoBlock2 ->
    [ bind_
      ~: Definition
    , "f"
      ~: Definition
    ]

  Expression DoBlock3 ->
    [ bind
      ~: Definition
    , "f"
      ~: Definition
    ]

  Expression DoBlock4 ->
    [ bind
      ~: Definition
    , bind_
      ~: Definition
    , "f"
      ~: Definition
    ]

  Declaration TypeSig ->
    [ "g"
      ~: Definition
    , "h"
      ~: Definition
    ]

  Declaration FunClause ->
    [ private "z"
      ~: Definition
    , "f"
      ~: Definition
    , "snoc"
      ~: Definition
    ]

  Declaration DataSig ->
    [ "D"
      ~: Data
    ]

  Declaration RecordDef ->
    [ "x"
      ~: Definition
    ]

  Declaration Syntax ->
    [ "p1"
      ~: Definition
    , "p1'"
      ~: Definition
    ]

  Declaration PatternSyn ->
    [ "q"
      ~: PatternSynonym
    , "f"
      ~: Definition
    , "g"
      ~: Definition
    ]

  Declaration Mutual1 ->
    [ "_"
      ~: Mutual
    ]

  Declaration Mutual2 ->
    [ "is-even'"
      ~: Definition
    ]

  Declaration Postulate ->
    [ "g"
      ~: Definition
    , "h"
      ~: Definition
    ]

  Declaration Open' ->
    [ private "N"
      ~: Open
    , private "O"
      ~: Open
    , "x"
      ~: Definition
    , "y"
      ~: Definition
    ]

  Declaration ModuleMacro ->
    [ private "x"
      ~: Variable
    , "Q"
      ~: Module
    , "S"
      ~: Module
    , "C"
      ~: Definition
    , "y"
      ~: Definition
    ]

  Declaration Module' ->
    [ "O"
      ~: Module
    , "P"
      ~: Module
    , "x"
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
  >> it "checks data signatures (DataSig)"
    (testCheck (Declaration DataSig))
  >> it "checks record definitions (RecordDef)"
    (testCheck (Declaration RecordDef))
  >> it "checks syntax declarations (Syntax)"
    (testCheck (Declaration Syntax))
  >> it "checks pattern synonyms (PatternSyn)"
    (testCheck (Declaration PatternSyn))
  >> it "checks mutual blocks (Mutual)"
    (testCheck (Declaration Mutual1)
    >> testCheck (Declaration Mutual2))
  >> it "checks postulates (Postulate)"
    (testCheck (Declaration Postulate))
  >> it "checks open statements (Open)"
    (testCheck (Declaration Open'))
  >> it "checks module macros (ModuleMacro)"
    (testCheck (Declaration ModuleMacro))
  >> it "checks module definitions (Module)"
    (testCheck (Declaration Module'))

