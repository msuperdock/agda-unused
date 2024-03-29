{- |
Module: Agda.Unused

Check an Agda project for unused code.
-}
module Agda.Unused.Check
  ( checkUnused
  , checkUnusedGlobal
  , checkUnusedWith
  ) where

import Agda.Unused
  (Unused(..), UnusedItems(..), UnusedOptions(..))
import Agda.Unused.Monad.Error
  (Error(..), InternalError(..), UnexpectedError(..), UnsupportedError(..),
    liftLookup)
import Agda.Unused.Monad.Reader
  (Environment(..), Mode(..), askGlobalMain, askIncludes, askLocal, askRoot,
    askSkip, localGlobal, localSkip)
import Agda.Unused.Monad.State
  (ModuleState(..), State, getHash, getModule, getSources, modifyBlock,
    modifyCheck, modifyDelete, modifyInsert, modifySources, stateEmpty,
    stateItems, stateModules)
import Agda.Unused.Types.Access
  (Access(..), fromAccess)
import Agda.Unused.Types.Context
  (AccessContext, AccessModule(..), Context, LookupError(..),
    accessContextConstructor, accessContextDefine, accessContextDefineFields,
    accessContextField, accessContextImport, accessContextItem,
    accessContextLookup, accessContextLookupDefining, accessContextLookupModule,
    accessContextLookupSpecial, accessContextMatch, accessContextModule,
    accessContextModule', accessContextPattern, accessContextRanges,
    accessContextUnion, contextDelete, contextDeleteModule, contextItem,
    contextLookupItem, contextLookupModule, contextModule, contextRanges,
    fromContext, moduleRanges, toContext)
import qualified Agda.Unused.Types.Context
  as C
import Agda.Unused.Types.Name
  (Name(..), QName(..), fromAsName, fromModuleName, fromName, fromNameRange,
    fromQName, fromQNameRange, nameIds, pathQName, qNamePath, toQName)
import Agda.Unused.Types.Range
  (RangeInfo(..), RangeType(..), rangePath)
import Agda.Unused.Utils
  (liftMaybe, mapLeft)

import Agda.Interaction.FindFile
  (findFile'', srcFilePath)
import Agda.Interaction.Options
  (CommandLineOptions(..), defaultOptions)
import Agda.Syntax.Common
  (Arg(..), Fixity'(..), ImportDirective'(..), ImportedName'(..),
    Named(..), NotationPart(..), Ranged(..), RecordDirectives'(..), Renaming'(..),
    RewriteEqn'(..), Using'(..), namedThing, unArg)
import qualified Agda.Syntax.Common
  as Common
import Agda.Syntax.Concrete
  (Binder, Binder'(..), BoundName(..), Declaration, DoStmt(..), Expr(..),
    FieldAssignment, FieldAssignment'(..), ImportDirective, ImportedName,
    LamBinding, LamBinding'(..), LamClause(..), LHS(..), Module(..),
    ModuleApplication(..), ModuleAssignment(..), OpenShortHand(..), Pattern(..),
    RecordAssignment, RecordDirectives, Renaming, RewriteEqn, RHS, RHS'(..),
    TypedBinding, TypedBinding'(..), WhereClause, WhereClause'(..), _exprFieldA)
import qualified Agda.Syntax.Concrete
  as Concrete
import Agda.Syntax.Concrete.Definitions
  (Clause(..), NiceConstructor, NiceDeclaration(..), niceDeclarations, runNice)
import Agda.Syntax.Concrete.Fixity
  (DoWarn(..), Fixities, fixitiesAndPolarities)
import Agda.Syntax.Concrete.Name
  (NameInScope(..), NamePart(..), nameRange)
import qualified Agda.Syntax.Concrete.Name
  as N
import Agda.Syntax.Parser
  (moduleParser, parseFile, runPMIO)
import Agda.Syntax.Position
  (Range, Range'(..), RangeFile(..), getRange)
import Agda.Syntax.TopLevelModuleName
  (RawTopLevelModuleName, TopLevelModuleName, projectRoot,
    rawTopLevelModuleNameForModule, rawTopLevelModuleNameForQName,
    unsafeTopLevelModuleName)
import Agda.TypeChecking.Monad.Base
  (TCM, runTCMTop)
import Agda.TypeChecking.Monad.Options
  (getIncludeDirs, setCommandLineOptions)
import Agda.Utils.FileName
  (filePath, mkAbsolute)
import qualified Agda.Utils.List2
  as List2
import Agda.Utils.List2
  (List2(..))
import Control.Monad
  (foldM, unless, void, when)
import Control.Monad.Except
  (ExceptT, MonadError, liftEither, runExceptT, throwError)
import Control.Monad.IO.Class
  (MonadIO, liftIO)
import Control.Monad.Reader
  (MonadReader, runReaderT)
import Control.Monad.State
  (MonadState, runStateT)
import Data.Bool
  (bool)
import Data.Foldable
  (traverse_)
import qualified Data.List.NonEmpty
  as NonEmpty
import Data.List.NonEmpty
  (NonEmpty(..), nonEmpty)
import qualified Data.Map.Strict
  as Map
import Data.Maybe
  (catMaybes)
import Data.Set
  (Set)
import qualified Data.Set
  as Set
import qualified Data.Text
  as T
import System.Directory
  (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath
  ((</>))

-- ## Context

-- Do nothing if `askSkip` returns true.
contextInsertRangeAll
  :: MonadReader Environment m
  => Range
  -> Context
  -> m Context
contextInsertRangeAll r c
  = askSkip >>= pure . bool (C.contextInsertRangeAll r c) c

-- Do nothing if `askSkip` returns true.
accessContextInsertRangeAll
  :: MonadReader Environment m
  => Range
  -> AccessContext
  -> m AccessContext
accessContextInsertRangeAll r c
  = askSkip >>= pure . bool (C.accessContextInsertRangeAll r c) c

-- ## Syntax

syntax
  :: Fixities
  -> Name
  -> Maybe Name
syntax fs (Name ps)
  = Map.lookup (N.Name NoRange InScope ps) fs >>= fromFixity

fromFixity
  :: Fixity'
  -> Maybe Name
fromFixity (Fixity' _ ps _)
  = Name <$> nonEmpty (fromNotationPart <$> ps)

fromNotationPart
  :: NotationPart
  -> NamePart
fromNotationPart (IdPart (Ranged _ s))
  = Id s
fromNotationPart (HolePart _ _)
  = Hole
fromNotationPart (VarPart _ _)
  = Hole
fromNotationPart (WildPart _)
  = Hole

-- ## Lists

checkSequence
  :: Monad m
  => Monoid c
  => (a -> b -> m c)
  -> a
  -> [b]
  -> m c
checkSequence f x ys
  = mconcat <$> sequence (f x <$> ys)

checkSequence1
  :: Monad m
  => Monoid c
  => (a -> b -> m c)
  -> a
  -> NonEmpty b
  -> m c
checkSequence1 f x ys
  = checkSequence f x (NonEmpty.toList ys)

checkFold
  :: Monad m
  => Monoid a
  => (a -> b -> m a)
  -> a
  -> [b]
  -> m a
checkFold
  = checkFoldWith mempty (<>)

checkFold1
  :: Monad m
  => Monoid a
  => (a -> b -> m a)
  -> a
  -> NonEmpty b
  -> m a
checkFold1 f x ys
  = checkFold f x (NonEmpty.toList ys)

checkFoldUnion
  :: Monad m
  => (AccessContext -> b -> m AccessContext)
  -> AccessContext
  -> [b]
  -> m AccessContext
checkFoldUnion
  = checkFoldWith mempty accessContextUnion

checkFoldWith
  :: Monad m
  => a
  -> (a -> a -> a)
  -> (a -> b -> m a)
  -> a
  -> [b]
  -> m a
checkFoldWith x f g x' ys
  = foldM (\x'' y -> f x'' <$> g (f x' x'') y) x ys

-- ## Names

checkName
  :: MonadReader Environment m
  => MonadState State m
  => Bool
  -- ^ Whether to treat names as pattern synonyms.
  -> Fixities
  -> Access
  -> RangeType
  -> Range
  -> Name
  -> m AccessContext
checkName _ _ _ _ NoRange _
  = pure mempty
checkName _ _ _ _ _ (Name (Hole :| []))
  = pure mempty
checkName p fs a t r@(Range _ _) n
  = modifyInsert r (RangeNamed t (QName n))
  >> pure (bool accessContextItem accessContextPattern p n a (Set.singleton r)
    (syntax fs n))

checkName'
  :: MonadReader Environment m
  => MonadState State m
  => Bool
  -- ^ Whether to treat names as pattern synonyms.
  -> Fixities
  -> Access
  -> RangeType
  -> N.Name
  -> m AccessContext
checkName' p fs a t n
  = maybe (pure mempty) (uncurry (checkName p fs a t)) (fromNameRange n)

checkNames'
  :: MonadReader Environment m
  => MonadState State m
  => Bool
  -- ^ Whether to treat names as pattern synonyms.
  -> Access
  -> RangeType
  -> [N.Name]
  -> m AccessContext
checkNames' p a
  = checkSequence (checkName' p mempty a)

checkQNameP
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => AccessContext
  -> Range
  -> QName
  -> m AccessContext
checkQNameP c r n
  = checkQNamePWith (accessContextLookupSpecial n c) c r n

checkQNamePWith
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => Maybe Bool
  -> AccessContext
  -> Range
  -> QName
  -> m AccessContext
checkQNamePWith Nothing _ r n
  = checkQName Public RangeVariable r n
checkQNamePWith (Just False) _ _ _
  = pure mempty
checkQNamePWith (Just True) c r n
  = touchQName c r n >> pure mempty

checkQName
  :: MonadReader Environment m
  => MonadState State m
  => Access
  -> RangeType
  -> Range
  -> QName
  -> m AccessContext
checkQName a t r (QName n)
  = checkName False mempty a t r n
checkQName _ _ _ (Qual _ _)
  = pure mempty

checkModuleName
  :: MonadReader Environment m
  => MonadState State m
  => Context
  -> Access
  -> Range
  -> Name
  -> m AccessContext
checkModuleName c a r n
  = modifyInsert r (RangeNamed RangeModule (QName n))
  >> contextInsertRangeAll r c
  >>= \c' -> pure (accessContextModule n (AccessModule a (Set.singleton r) c'))

checkModuleNameMay
  :: MonadReader Environment m
  => MonadState State m
  => Context
  -> Access
  -> Range
  -> Maybe Name
  -- ^ If `Nothing`, the module is anonymous.
  -> m AccessContext
checkModuleNameMay _ _ _ Nothing
  = pure mempty
checkModuleNameMay c a r (Just n)
  = checkModuleName c a r n

touchName
  :: MonadReader Environment m
  => MonadState State m
  => AccessContext
  -> Name
  -> m ()
touchName c n
  = askSkip >>= touchNameWith (accessContextLookupDefining (QName n) c)

touchNameWith
  :: MonadReader Environment m
  => MonadState State m
  => Either LookupError (Bool, Set Range)
  -> Bool
  -> m ()
touchNameWith (Right (False, rs)) False
  = modifyDelete rs
touchNameWith _ _
  = pure ()

touchNames
  :: MonadReader Environment m
  => MonadState State m
  => AccessContext
  -> [Name]
  -> m ()
touchNames
  = checkSequence touchName

touchQName
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => AccessContext
  -> Range
  -> QName
  -> m ()
touchQName c r n
  = askSkip >>= touchQNameWith r n (accessContextLookupDefining n c)

touchQNameWith
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => Range
  -> QName
  -> Either LookupError (Bool, Set Range)
  -> Bool
  -> m ()
touchQNameWith r n (Left LookupAmbiguous) False
  = throwError (ErrorAmbiguous r n)
touchQNameWith _ _ rs b
  = touchNameWith rs b

touchQName'
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => AccessContext
  -> N.QName
  -> m ()
touchQName' c n
  = traverse_ (uncurry (touchQName c)) (fromQNameRange n)

-- ## Bindings

checkBinder
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Bool
  -- ^ Whether to check bound names.
  -> AccessContext
  -> Binder
  -> m AccessContext
checkBinder b c (Binder p (BName n _ _ _))
  = bool localSkip id b (checkName' False mempty Public RangeVariable n)
  >>= \c' -> checkPatternMay c p
  >>= \c'' -> pure (c' <> c'')

checkBinders1
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Bool
  -- ^ Whether to check bound names.
  -> AccessContext
  -> NonEmpty Binder
  -> m AccessContext
checkBinders1 b
  = checkSequence1 (checkBinder b)

checkLamBinding
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Bool
  -- ^ Whether to check bound names.
  -> AccessContext
  -> LamBinding
  -> m AccessContext
checkLamBinding b c (DomainFree (Arg _ (Named _ b')))
  = checkBinder b c b'
checkLamBinding b c (DomainFull b')
  = checkTypedBinding b c b'

checkLamBindings
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Bool
  -- ^ Whether to check bound names.
  -> AccessContext
  -> [LamBinding]
  -> m AccessContext
checkLamBindings b
  = checkFold (checkLamBinding b)

checkLamBindings1
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Bool
  -- ^ Whether to check bound names.
  -> AccessContext
  -> NonEmpty LamBinding
  -> m AccessContext
checkLamBindings1 b
  = checkFold1 (checkLamBinding b)

checkTypedBinding
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Bool
  -- ^ Whether to check bound names.
  -> AccessContext
  -> TypedBinding
  -> m AccessContext
checkTypedBinding b c (TBind _ bs e)
  = checkExpr c e >> checkBinders1 b c (namedThing . unArg <$> bs)
checkTypedBinding _ c (TLet _ ds)
  = checkDeclarations1 c ds

checkTypedBindings
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Bool
  -- ^ Whether to check bound names.
  -> AccessContext
  -> [TypedBinding]
  -> m AccessContext
checkTypedBindings b
  = checkFold (checkTypedBinding b)

checkTypedBindings1
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Bool
  -- ^ Whether to check bound names.
  -> AccessContext
  -> NonEmpty TypedBinding
  -> m AccessContext
checkTypedBindings1 b
  = checkFold1 (checkTypedBinding b)

-- ## Patterns

data PatternRec m
  = PatternRec
  { checkIdentP
    :: AccessContext
    -> Range
    -> QName
    -> m AccessContext
  , checkAsP
    :: Range
    -> Name
    -> m AccessContext
  , checkRawAppP
    :: (AccessContext -> Pattern -> m AccessContext)
    -> AccessContext
    -> [Pattern]
    -> m AccessContext
  }

checkPatternRec
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => PatternRec m
  -> AccessContext
  -> Pattern
  -> m AccessContext
checkPatternRec r c (IdentP n)
  = maybe (pure mempty) (uncurry (checkIdentP r c)) (fromQNameRange n)
checkPatternRec _ _ (QuoteP _)
  = pure mempty
checkPatternRec r c (AppP p (Arg _ (Named _ p')))
  = (<>) <$> checkPatternRec r c p
    <*> checkPatternRec r c p'
checkPatternRec r c (RawAppP _ ps)
  = checkRawAppP r (checkPatternRec r) c (List2.toList ps)
checkPatternRec _ _ (OpAppP r _ _ _)
  = throwError (ErrorInternal (ErrorUnexpected UnexpectedOpAppP r))
checkPatternRec r c (HiddenP _ (Named _ p))
  = checkPatternRec r c p
checkPatternRec r c (InstanceP _ (Named _ p))
  = checkPatternRec r c p
checkPatternRec r c (ParenP _ p)
  = checkPatternRec r c p
checkPatternRec _ _ (WildP _)
  = pure mempty
checkPatternRec _ _ (AbsurdP _)
  = pure mempty
checkPatternRec r c (AsP _ n p)
  = (<>) <$> maybe (pure mempty) (uncurry (checkAsP r)) (fromNameRange n)
    <*> checkPatternRec r c p
checkPatternRec _ c (DotP _ e)
  = checkExpr c e >> pure mempty
checkPatternRec _ _ (LitP _ _)
  = pure mempty
checkPatternRec _ c (RecP _ as)
  = checkSequence checkPattern c (_exprFieldA <$> as)
checkPatternRec _ c (EqualP _ es)
  = checkExprPairs c es >> pure mempty
checkPatternRec _ _ (EllipsisP _ _)
  = pure mempty
checkPatternRec r c (WithP _ p)
  = checkPatternRec r c p

patternRec
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => PatternRec m
patternRec
  = PatternRec
  { checkIdentP
    = checkQNameP
  , checkAsP
    = checkName False mempty Public RangeVariable
  , checkRawAppP
    = \f c ps -> pure (accessContextMatch (patternNames ps) c)
    >>= \ns -> touchNames c ns
    >> checkSequence f c (patternDelete ns ps)
  }

checkPattern
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> Pattern
  -> m AccessContext
checkPattern
  = checkPatternRec patternRec

checkIdentPLet
  :: MonadReader Environment m
  => MonadState State m
  => Maybe Name
  -> AccessContext
  -> Range
  -> QName
  -> m AccessContext
checkIdentPLet (Just n) _ _ (QName n') | n == n'
  = pure mempty
checkIdentPLet _ _ r n
  = checkQName Public RangeVariable r n

patternRecLet
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => Maybe Name
  -> PatternRec m
patternRecLet n
  = PatternRec
  { checkIdentP
    = checkIdentPLet n
  , checkAsP
    = \r _ -> throwError (ErrorInternal (ErrorLet r))
  , checkRawAppP
    = checkSequence
  }

checkPatternLet
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Maybe Name
  -- ^ A name to ignore.
  -> Pattern
  -> m AccessContext
checkPatternLet n
  = checkPatternRec (patternRecLet n) mempty

checkPatternMay
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> Maybe Pattern
  -> m AccessContext
checkPatternMay _ Nothing
  = pure mempty
checkPatternMay c (Just p)
  = checkPattern c p

checkPatterns
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> [Pattern]
  -> m AccessContext
checkPatterns
  = checkSequence checkPattern

patternMatch
  :: [Name]
  -> Pattern
  -> Bool
patternMatch ns p
  = maybe False (flip elem (concat (nameIds <$> ns))) (patternName p)

patternDelete
  :: [Name]
  -> [Pattern]
  -> [Pattern]
patternDelete ns
  = filter (not . patternMatch ns)

patternName
  :: Pattern
  -> Maybe String
patternName (IdentP (N.QName (N.Name _ _ (Id n :| []))))
  = Just n
patternName _
  = Nothing

patternNames
  :: [Pattern]
  -> [String]
patternNames
  = catMaybes . fmap patternName

-- ## Expressions

checkExpr
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> Expr
  -> m ()
checkExpr c (Ident n)
  = touchQName' c n
checkExpr _ (Lit _ _)
  = pure ()
checkExpr _ (QuestionMark _ _)
  = pure ()
checkExpr _ (Underscore _ _)
  = pure ()
checkExpr c (RawApp _ es)
  = checkRawApp c (List2.toList es)
checkExpr c (App _ e (Arg _ (Named _ e')))
  = checkExpr c e >> checkExpr c e'
checkExpr _ (OpApp r _ _ _)
  = throwError (ErrorInternal (ErrorUnexpected UnexpectedOpApp r))
checkExpr c (WithApp _ e es)
  = checkExpr c e >> checkExprs c es
checkExpr c (HiddenArg _ (Named _ e))
  = checkExpr c e
checkExpr c (InstanceArg _ (Named _ e))
  = checkExpr c e
checkExpr c (Lam _ bs e)
  = checkLamBindings1 True c bs >>= \c' -> checkExpr (c <> c') e
checkExpr _ (AbsurdLam _ _)
  = pure ()
checkExpr c (ExtendedLam _ _ ls)
  = checkLamClauses1_ c ls
checkExpr c (Fun _ (Arg _ e) e')
  = checkExpr c e >> checkExpr c e'
checkExpr c (Pi bs e)
  = checkTypedBindings1 True c bs >>= \c' -> checkExpr (c <> c') e
checkExpr c (Rec _ rs)
  = checkRecordAssignments c rs
checkExpr c (RecUpdate _ e fs)
  = checkExpr c e >> checkFieldAssignments c fs
checkExpr c (Let _ ds e)
  = checkDeclarationsLet1 c ds >>= \c' -> traverse_ (checkExpr (c <> c')) e
checkExpr c (Paren _ e)
  = checkExpr c e
checkExpr c (IdiomBrackets _ es)
  = checkExprs c es
checkExpr c (DoBlock _ ss)
  = void (checkDoStmts1 c ss)
checkExpr _ (Absurd r)
  = throwError (ErrorInternal (ErrorUnexpected UnexpectedAbsurd r))
checkExpr _ (As r _ _)
  = throwError (ErrorInternal (ErrorUnexpected UnexpectedAs r))
checkExpr c (Dot _ e)
  = checkExpr c e
checkExpr c (DoubleDot _ e)
  = checkExpr c e
checkExpr _ (Quote _)
  = pure ()
checkExpr _ (QuoteTerm _)
  = pure ()
checkExpr c (Tactic _ e)
  = checkExpr c e
checkExpr _ (Unquote _)
  = pure ()
checkExpr _ e@(DontCare _)
  = throwError (ErrorInternal (ErrorUnexpected UnexpectedDontCare (getRange e)))
checkExpr _ (Equal r _ _)
  = throwError (ErrorInternal (ErrorUnexpected UnexpectedEqual r))
checkExpr _ (Ellipsis r)
  = throwError (ErrorInternal (ErrorUnexpected UnexpectedEllipsis r))
checkExpr c (Generalized e)
  = checkExpr c e

checkExprs
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> [Expr]
  -> m ()
checkExprs
  = checkSequence checkExpr

checkExprs1
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> NonEmpty Expr
  -> m ()
checkExprs1
  = checkSequence1 checkExpr

checkExprPair
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> (Expr, Expr)
  -> m ()
checkExprPair c (e1, e2)
  = checkExpr c e1 >> checkExpr c e2

checkExprPairs
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> [(Expr, Expr)]
  -> m ()
checkExprPairs
  = checkSequence checkExprPair

checkRawApp
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> [Expr]
  -> m ()
checkRawApp c es
  = touchNames c (accessContextMatch (exprNames es) c)
  >> checkExprs c es

exprName
  :: Expr
  -> Maybe String
exprName (Ident (N.QName (N.Name _ _ (Id n :| []))))
  = Just n
exprName _
  = Nothing

exprNames
  :: [Expr]
  -> [String]
exprNames
  = catMaybes . fmap exprName

-- ## Assignments

checkRecordAssignment
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> RecordAssignment
  -> m ()
checkRecordAssignment c (Left f)
  = checkFieldAssignment c f
checkRecordAssignment c (Right m)
  = checkModuleAssignment c m

checkRecordAssignments
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> [RecordAssignment]
  -> m ()
checkRecordAssignments
  = checkSequence checkRecordAssignment

checkFieldAssignment
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> FieldAssignment
  -> m ()
checkFieldAssignment c (FieldAssignment _ e)
  = checkExpr c e

checkFieldAssignments
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> [FieldAssignment]
  -> m ()
checkFieldAssignments
  = checkSequence checkFieldAssignment

checkModuleAssignment
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> ModuleAssignment
  -> m ()
checkModuleAssignment c a@(ModuleAssignment n es _)
  = checkExprs c es
  >> traverse_ (touchModule c (getRange a)) (fromQName n)

-- ## Definitions

checkLHS
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> LHS
  -> m AccessContext
checkLHS c (LHS p rs ws)
  = checkPattern c p
  >>= \c' -> checkRewriteEqns (c <> c') rs
  >>= \c'' -> checkExprs (c <> c' <> c'') (unArg . namedThing <$> ws)
  >> pure (c' <> c'')

checkRHS
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> RHS
  -> m ()
checkRHS _ AbsurdRHS
  = pure ()
checkRHS c (RHS e)
  = checkExpr c e

checkClause
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> Clause
  -> m AccessContext
checkClause c (Clause n _ l r w cs)
  = pure (maybe id accessContextDefine (fromName n) c)
  >>= \c' -> checkLHS c' l
  >>= \c'' -> checkWhereClause (c' <> c'') w
  >>= \(m, c''') -> checkRHS (c' <> c'' <> c''') r
  >> checkClauses (c' <> c'' <> c''') cs
  >>= \m' -> pure (m <> m')

checkClauses
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> [Clause]
  -> m AccessContext
checkClauses
  = checkSequence checkClause

checkLamClause
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> LamClause
  -> m AccessContext
checkLamClause c (LamClause ps r _)
  = checkPatterns c ps
  >>= \c' -> checkRHS (c <> c') r
  >> pure c'

checkLamClause_
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> LamClause
  -> m ()
checkLamClause_ c ls
  = void (checkLamClause c ls)

checkLamClauses
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> [LamClause]
  -> m AccessContext
checkLamClauses
  = checkFold checkLamClause

-- Do not propogate context from one clause to the next.
checkLamClauses1_
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> NonEmpty LamClause
  -> m ()
checkLamClauses1_
  = checkSequence1 checkLamClause_

checkWhereClause
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> WhereClause
  -> m (AccessContext, AccessContext)
  -- ^ A context for the named where block, then the full context.
checkWhereClause _ NoWhere
  = pure (mempty, mempty)
checkWhereClause c (AnyWhere _ ds)
  = checkDeclarations c ds
  >>= \c' -> pure (mempty, c')
checkWhereClause c (SomeWhere _ n a ds)
  = checkDeclarations c ds
  >>= \c' -> checkModuleNameMay (toContext c') (fromAccess a) (getRange n)
    (fromName n)
  >>= \c'' -> pure (c'' , c')

checkRewriteEqn
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> RewriteEqn
  -> m AccessContext
checkRewriteEqn c (Rewrite rs)
  = checkExprs1 c (snd <$> rs) >> pure mempty
checkRewriteEqn c (Invert _ ws)
  = checkIrrefutableWiths c (namedThing <$> ws)

checkRewriteEqns
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> [RewriteEqn]
  -> m AccessContext
checkRewriteEqns
  = checkFold checkRewriteEqn

checkIrrefutableWith
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> (Pattern, Expr)
  -> m AccessContext
checkIrrefutableWith c (p, e)
  = checkExpr c e >> checkPattern c p

checkIrrefutableWiths
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> NonEmpty (Pattern, Expr)
  -> m AccessContext
checkIrrefutableWiths
  = checkFold1 checkIrrefutableWith

checkDoStmt
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> DoStmt
  -> m AccessContext
checkDoStmt c (DoBind _ p e cs)
  = checkLamClauses c cs
  >>= \c' -> checkExpr (c <> c') e
  >> checkPattern (c <> c') p
  >>= \c'' -> pure (c' <> c'')
checkDoStmt c (DoThen e)
  = checkExpr c e >> pure mempty
checkDoStmt c (DoLet _ ds)
  = checkDeclarations1 c ds

checkDoStmts1
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> NonEmpty DoStmt
  -> m AccessContext
checkDoStmts1 c (s :| ss)
  = when (hasBind s ss) (touchName c bind)
  >> when (hasBind_ s ss) (touchName c bind_)
  >> checkFold checkDoStmt c (s : ss)

hasBind
  :: DoStmt
  -> [DoStmt]
  -> Bool
hasBind _ []
  = False
hasBind (DoBind _ _ _ _) (_ : _)
  = True
hasBind _ (s : ss)
  = hasBind s ss

hasBind_
  :: DoStmt
  -> [DoStmt]
  -> Bool
hasBind_ _ []
  = False
hasBind_ (DoThen _) (_ : _)
  = True
hasBind_ _ (s : ss)
  = hasBind_ s ss

bind
  :: Name
bind
  = Name
  $ (:|) Hole
  $ (:) (Id ">>=")
  $ (:) Hole
  $ []

bind_
  :: Name
bind_
  = Name
  $ (:|) Hole
  $ (:) (Id ">>")
  $ (:) Hole
  $ []

-- ## Declarations

checkDeclarations
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> [Declaration]
  -> m AccessContext
checkDeclarations
  = checkDeclarationsWith checkNiceDeclarations

checkDeclarations1
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> NonEmpty Declaration
  -> m AccessContext
checkDeclarations1 c ds
  = checkDeclarations c (NonEmpty.toList ds)

checkDeclarationsRecord
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Name
  -> Set Range
  -- ^ Ranges associated with parent record.
  -> AccessContext
  -> [Declaration]
  -> m AccessContext
checkDeclarationsRecord n rs
  = checkDeclarationsWith (checkNiceDeclarationsRecord n rs)

checkDeclarationsLet
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> [Declaration]
  -> m AccessContext
checkDeclarationsLet
  = checkDeclarationsWith checkNiceDeclarationsLet

checkDeclarationsLet1
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> NonEmpty Declaration
  -> m AccessContext
checkDeclarationsLet1 c ds
  = checkDeclarationsLet c (NonEmpty.toList ds)

checkDeclarationsTop
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> [Declaration]
  -> m AccessContext
checkDeclarationsTop
  = checkDeclarationsWith checkNiceDeclarationsTop

checkDeclarationsWith
  :: MonadError Error m
  => (Fixities -> AccessContext -> [NiceDeclaration] -> m AccessContext)
  -> AccessContext
  -> [Declaration]
  -> m AccessContext
checkDeclarationsWith f c ds = do
  (fixities, _)
    <- fixitiesAndPolarities NoWarn ds
  (niceDeclsEither, _) 
    <- pure (runNice (niceDeclarations fixities ds))
  niceDecls
    <- liftEither (mapLeft ErrorDeclaration niceDeclsEither)
  f fixities c niceDecls

checkNiceDeclaration
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Fixities
  -> AccessContext
  -> NiceDeclaration
  -> m AccessContext
checkNiceDeclaration fs c d
  = askGlobalMain
  >>= checkNiceDeclarationWith fs c d

checkNiceDeclarationWith
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Fixities
  -> AccessContext
  -> NiceDeclaration
  -> Bool
  -> m AccessContext
checkNiceDeclarationWith fs c d False
  = checkNiceDeclaration' fs c d
checkNiceDeclarationWith fs c d@(NiceImport _ _ _ _ _) True
  = localGlobal (checkNiceDeclaration' fs c d)
  >>= \c' -> touchAccessContext c'
  >> pure c'
checkNiceDeclarationWith _ _ d True
  = throwError (ErrorGlobal (getRange d))

checkNiceDeclaration'
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Fixities
  -> AccessContext
  -> NiceDeclaration
  -> m AccessContext

checkNiceDeclaration' fs c (Axiom _ a _ _ _ n e)
  = checkExpr c e >> checkName' False fs (fromAccess a) RangePostulate n
checkNiceDeclaration' _ _ (NiceField r _ _ _ _ _ _)
  = throwError (ErrorInternal (ErrorUnexpected UnexpectedField r))
checkNiceDeclaration' fs c (PrimitiveFunction _ a _ n (Arg _ e))
  = checkExpr c e >> checkName' False fs (fromAccess a) RangeDefinition n
checkNiceDeclaration' _ c (NiceModule r a _ (N.QName n) bs ds)
  = checkNiceModule c (fromAccess a) r (fromName n) bs ds
checkNiceDeclaration' _ _ (NiceModule _ _ _ n@(N.Qual _ _) _ _)
  = throwError (ErrorInternal (ErrorName (getRange n)))
checkNiceDeclaration' _ c (NiceModuleMacro r a n m o i)
  = checkNiceModuleMacro c (fromAccess a) r n m o i
checkNiceDeclaration' _ _ (NicePragma _ _)
  = pure mempty
checkNiceDeclaration' fs c (NiceRecSig _ a _ _ _ n bs e)
  = checkNiceSig fs c a RangeRecord n bs e
checkNiceDeclaration' fs c (NiceDataSig _ a _ _ _ n bs e)
  = checkNiceSig fs c a RangeData n bs e
checkNiceDeclaration' _ _ (NiceFunClause r _ _ _ _ _ _)
  = throwError (ErrorInternal (ErrorUnexpected UnexpectedNiceFunClause r))
checkNiceDeclaration' fs c (FunSig _ a _ _ _ _ _ _ n e)
  = checkExpr c e >> checkName' False fs (fromAccess a) RangeDefinition n
checkNiceDeclaration' _ c (FunDef _ _ _ _ _ _ _ cs)
  = checkClauses c cs >> pure mempty
checkNiceDeclaration' fs c (NiceDataDef _ _ _ _ _ n bs cs)
  = checkNiceDataDef True fs c n bs cs
checkNiceDeclaration' _ _ (NiceLoneConstructor r _)
  = throwError (ErrorUnsupported UnsupportedLoneConstructor r)
checkNiceDeclaration' fs c (NiceRecDef _ _ _ _ _ n rs bs ds)
  = checkNiceRecordDef True fs c n rs bs ds
checkNiceDeclaration' _ c (NiceGeneralize _ _ _ _ _ e)
  = checkExpr c e >> pure mempty
checkNiceDeclaration' _ _ (NiceUnquoteDecl r _ _ _ _ _ _ _)
  = throwError (ErrorUnsupported UnsupportedUnquote r)
checkNiceDeclaration' _ _ (NiceUnquoteDef r _ _ _ _ _ _)
  = throwError (ErrorUnsupported UnsupportedUnquote r)
checkNiceDeclaration' _ _ (NiceUnquoteData r _ _ _ _ _ _ _)
  = throwError (ErrorUnsupported UnsupportedUnquote r)

checkNiceDeclaration' fs c
  (NiceMutual _ _ _ _
    (d@(NiceRecSig _ _ _ _ _ n _ _) : NiceRecDef _ _ _ _ _ n' rs bs ds : []))
  | nameRange n == nameRange n'
  = checkNiceDeclaration fs c d
  >>= \c' -> checkNiceRecordDef False fs (c <> c') n' rs bs ds
  >>= \c'' -> pure (c' <> c'')
checkNiceDeclaration' fs c
  (NiceMutual _ _ _ _
    (d@(NiceDataSig _ _ _ _ _ n _ _) : NiceDataDef _ _ _ _ _ n' bs cs : []))
  | nameRange n == nameRange n'
  = checkNiceDeclaration fs c d
  >>= \c' -> checkNiceDataDef False fs (c <> c') n' bs cs
  >>= \c'' -> pure (c' <> c'')
checkNiceDeclaration' fs c
  (NiceMutual _ _ _ _
    ds@(FunSig _ _ _ _ _ _ _ _ _ _ : FunDef _ _ _ _ _ _ _ _ : []))
  = checkNiceDeclarations fs c ds
checkNiceDeclaration' fs c (NiceMutual r _ _ _ ds)
  = checkNiceDeclarations fs c ds
  >>= \c' -> modifyInsert r RangeMutual
  >> accessContextInsertRangeAll r c'

checkNiceDeclaration' _ c (NiceOpen r n i)
  = liftMaybe (ErrorInternal (ErrorName (getRange n))) (fromQName n)
  >>= \n' -> liftLookup r n' (accessContextLookupModule n' c)
  >>= \(C.Module rs c') -> modifyDelete rs
  >> checkImportDirective Open r n' c' i
  >>= \c'' -> pure (fromContext (importDirectiveAccess i) c'')

checkNiceDeclaration' _ _ (NiceImport r n Nothing DontOpen i)
  = liftMaybe (ErrorInternal (ErrorName (getRange n))) (fromQName n)
  >>= \n' -> checkFile r n'
  >>= \c' -> checkImportDirective Import r n' c' i
  >>= \c'' -> pure (accessContextImport n' c'')
checkNiceDeclaration' _ _ (NiceImport r n Nothing DoOpen i)
  = liftMaybe (ErrorInternal (ErrorName (getRange n))) (fromQName n)
  >>= \n' -> checkFile r n'
  >>= \c' -> checkImportDirective Import r n' c' i
  >>= \c'' -> pure (accessContextImport n' c'
    <> fromContext (importDirectiveAccess i) c'')
checkNiceDeclaration' _ _ (NiceImport r n (Just a) DontOpen i)
  = liftMaybe (ErrorInternal (ErrorName (getRange n))) (fromQName n)
  >>= \n' -> checkFile r n'
  >>= \c' -> checkImportDirective Import r n' c' i
  >>= \c'' -> checkModuleNameMay c'' Public (getRange a) (fromAsName a)
checkNiceDeclaration' _ _ (NiceImport r n (Just a) DoOpen i)
  = liftMaybe (ErrorInternal (ErrorName (getRange n))) (fromQName n)
  >>= \n' -> checkFile r n'
  >>= \c' -> checkImportDirective Import r n' c' i
  >>= \c'' -> checkModuleNameMay c' Public (getRange a) (fromAsName a)
  >>= \c''' -> pure (c''' <> fromContext (importDirectiveAccess i) c'')

checkNiceDeclaration' fs c (NicePatternSyn _ a n ns p)
  = localSkip (checkNames' False Public RangeVariable (unArg <$> ns))
  >>= \c' -> checkPattern (c <> c') p
  >> checkName' True fs (fromAccess a) RangePatternSynonym n

checkNiceDeclarationRecord
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Name
  -> Set Range
  -- ^ Ranges associated with parent record.
  -> Fixities
  -> AccessContext
  -> NiceDeclaration
  -> m AccessContext
checkNiceDeclarationRecord _ rs fs c d@(Axiom _ _ _ _ _ _ _)
  = modifyDelete rs >> checkNiceDeclaration fs c d
checkNiceDeclarationRecord _ rs fs c d@(PrimitiveFunction _ _ _ _ _)
  = modifyDelete rs >> checkNiceDeclaration fs c d
checkNiceDeclarationRecord n rs fs c (NiceMutual _ _ _ _ ds)
  = checkNiceDeclarationsRecord n rs fs c ds
checkNiceDeclarationRecord _ rs fs c d@(NiceModule _ _ _ _ _ _)
  = modifyDelete rs >> checkNiceDeclaration fs c d
checkNiceDeclarationRecord _ _ fs c d@(NiceModuleMacro _ _ _ _ _ _)
  = checkNiceDeclaration fs c d
checkNiceDeclarationRecord _ _ fs c d@(NiceOpen _ _ _)
  = checkNiceDeclaration fs c d
checkNiceDeclarationRecord _ _ fs c d@(NiceImport _ _ _ _ _)
  = checkNiceDeclaration fs c d
checkNiceDeclarationRecord _ _ fs c d@(NicePragma _ _)
  = checkNiceDeclaration fs c d
checkNiceDeclarationRecord _ rs fs c d@(NiceRecSig _ _ _ _ _ _ _ _)
  = modifyDelete rs >> checkNiceDeclaration fs c d
checkNiceDeclarationRecord _ rs fs c d@(NiceDataSig _ _ _ _ _ _ _ _)
  = modifyDelete rs >> checkNiceDeclaration fs c d
checkNiceDeclarationRecord _ _ fs c d@(NiceFunClause _ _ _ _ _ _ _)
  = checkNiceDeclaration fs c d
checkNiceDeclarationRecord _ rs fs c d@(FunSig _ _ _ _ _ _ _ _ _ _)
  = modifyDelete rs >> checkNiceDeclaration fs c d
checkNiceDeclarationRecord _ _ fs c d@(FunDef _ _ _ _ _ _ _ _)
  = checkNiceDeclaration fs c d
checkNiceDeclarationRecord _ _ fs c d@(NiceDataDef _ _ _ _ _ _ _ _)
  = checkNiceDeclaration fs c d
checkNiceDeclarationRecord _ _ _ _ (NiceLoneConstructor r _)
  = throwError (ErrorUnsupported UnsupportedLoneConstructor r)
checkNiceDeclarationRecord _ _ fs c d@(NiceRecDef _ _ _ _ _ _ _ _ _)
  = checkNiceDeclaration fs c d
checkNiceDeclarationRecord _ _ fs c d@(NicePatternSyn _ _ _ _ _)
  = checkNiceDeclaration fs c d
checkNiceDeclarationRecord _ _ fs c d@(NiceGeneralize _ _ _ _ _ _)
  = checkNiceDeclaration fs c d
checkNiceDeclarationRecord _ _ fs c d@(NiceUnquoteDecl _ _ _ _ _ _ _ _)
  = checkNiceDeclaration fs c d
checkNiceDeclarationRecord _ _ fs c d@(NiceUnquoteDef _ _ _ _ _ _ _)
  = checkNiceDeclaration fs c d
checkNiceDeclarationRecord _ _ fs c d@(NiceUnquoteData _ _ _ _ _ _ _ _)
  = checkNiceDeclaration fs c d

checkNiceDeclarationRecord n rs fs c (NiceField _ a _ _ _ n' (Arg _ e))
  = checkExpr (accessContextDefine n (accessContextDefineFields c)) e
  >> maybe
    (pure mempty)
    (\n'' -> pure (accessContextField n'' (fromAccess a) rs (syntax fs n'')))
    (fromName n')

checkNiceDeclarationLet
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Fixities
  -> AccessContext
  -> NiceDeclaration
  -> m AccessContext
checkNiceDeclarationLet fs c
  (NiceMutual _ _ _ _
    (FunSig _ _ _ _ _ _ _ _ n e : FunDef _ _ _ _ _ _ _
      (Clause _ _ (LHS p [] []) r NoWhere [] : []) : []))
  = checkExpr c e
  >> checkPatternLet (fromName n) p
  >>= \c' -> checkRHS (c <> c') r
  >> checkName' False fs Public RangeDefinition n
checkNiceDeclarationLet fs c
  d@(NiceModuleMacro _ _ _ _ _ _)
  = checkNiceDeclaration fs c d
checkNiceDeclarationLet _ c
  (NiceFunClause _ _ _ _ _ _
    (Concrete.FunClause l r NoWhere _))
  = checkRHS c r
  >> checkLHS c l
checkNiceDeclarationLet _ _ d
  = throwError (ErrorInternal (ErrorLet (getRange d)))

checkNiceDeclarations
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Fixities
  -> AccessContext
  -> [NiceDeclaration]
  -> m AccessContext
checkNiceDeclarations fs
  = checkFoldUnion (checkNiceDeclaration fs)

checkNiceDeclarationsRecord
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Name
  -> Set Range
  -- ^ Ranges associated with parent record.
  -> Fixities
  -> AccessContext
  -> [NiceDeclaration]
  -> m AccessContext
checkNiceDeclarationsRecord n rs fs
  = checkFoldUnion (checkNiceDeclarationRecord n rs fs)

checkNiceDeclarationsLet
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Fixities
  -> AccessContext
  -> [NiceDeclaration]
  -> m AccessContext
checkNiceDeclarationsLet fs
  = checkFoldUnion (checkNiceDeclarationLet fs)

checkNiceDeclarationsTop
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Fixities
  -> AccessContext
  -> [NiceDeclaration]
  -> m AccessContext
checkNiceDeclarationsTop _ _ []
  = pure mempty
checkNiceDeclarationsTop _ c (NiceModule r a _ _ bs ds : _)
  = checkNiceModule c (fromAccess a) r Nothing bs ds
checkNiceDeclarationsTop fs c (d : ds)
  = checkNiceDeclaration fs c d
  >>= \c' -> checkNiceDeclarationsTop fs (c <> c') ds

checkNiceSig
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Fixities
  -> AccessContext
  -> Common.Access
  -> RangeType
  -> N.Name
  -> [LamBinding]
  -> Expr
  -> m AccessContext
checkNiceSig fs c a t n bs e
  = checkLamBindings False c bs
  >>= \c' -> checkExpr (c <> c') e
  >> checkName' False fs (fromAccess a) t n

checkNiceDataDef
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Bool
  -- ^ Whether to check bound names in lambda bindings.
  -> Fixities
  -> AccessContext
  -> N.Name
  -> [LamBinding]
  -> [NiceConstructor]
  -> m AccessContext
checkNiceDataDef b fs c n bs cs
  = liftMaybe (ErrorInternal (ErrorName (getRange n))) (fromName n)
  >>= \n' -> pure (either mempty id (accessContextLookup (QName n') c))
  >>= \rs -> checkLamBindings b c bs
  >>= \c' -> checkNiceConstructors fs rs (accessContextDefine n' c <> c') cs
  >>= \c'' -> pure (accessContextModule' n' Public rs c'' <> c'')

checkNiceRecordDef
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Bool
  -- ^ Whether to check bound names in lambda bindings.
  -> Fixities
  -> AccessContext
  -> N.Name
  -> RecordDirectives
  -> [LamBinding]
  -> [Declaration]
  -> m AccessContext
checkNiceRecordDef b fs c n (RecordDirectives _ _ _ m) bs ds
  = liftMaybe (ErrorInternal (ErrorName (getRange n))) (fromName n)
  >>= \n' -> pure (either (const mempty) id (accessContextLookup (QName n') c))
  >>= \rs' -> checkLamBindings b c bs
  >>= \c' -> checkNiceConstructorRecordMay fs rs' (m >>= fromNameRange . fst)
  >>= \c'' -> checkDeclarationsRecord n' rs' (c <> c') ds
  >>= \c''' -> pure (accessContextModule' n' Public rs' (c'' <> c''') <> c'')

checkNiceConstructor
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Fixities
  -> Set Range
  -- ^ Ranges associated with parent type.
  -> AccessContext
  -> NiceConstructor
  -> m AccessContext
checkNiceConstructor fs rs c (Axiom _ a _ _ _ n e)
  = checkExpr c e
  >> maybe
    (pure mempty)
    (\n'' -> pure (accessContextConstructor n'' (fromAccess a) rs
      (syntax fs n'')))
    (fromName n)
checkNiceConstructor _ _ _ d
  = throwError (ErrorInternal (ErrorConstructor (getRange d)))

checkNiceConstructors
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Fixities
  -> Set Range
  -- ^ Ranges associated with parent type.
  -> AccessContext
  -> [NiceDeclaration]
  -> m AccessContext
checkNiceConstructors fs rs
  = checkSequence (checkNiceConstructor fs rs)

checkNiceConstructorRecord
  :: MonadReader Environment m
  => MonadState State m
  => Fixities
  -> Set Range
  -- ^ Ranges associated with record type.
  -> Range
  -> Name
  -> m AccessContext
checkNiceConstructorRecord fs rs r n
  = modifyInsert r (RangeNamed RangeRecordConstructor (QName n))
  >> pure (accessContextConstructor n Public (Set.insert r rs) (syntax fs n))

checkNiceConstructorRecordMay
  :: MonadReader Environment m
  => MonadState State m
  => Fixities
  -> Set Range
  -- ^ Ranges associated with record type.
  -> Maybe (Range, Name)
  -> m AccessContext
checkNiceConstructorRecordMay _ _ Nothing
  = pure mempty
checkNiceConstructorRecordMay fs rs (Just (r, n))
  = checkNiceConstructorRecord fs rs r n

checkNiceModule
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> Access
  -> Range
  -> Maybe Name
  -- ^ If `Nothing`, the module is anonymous.
  -> [TypedBinding]
  -> [Declaration]
  -> m AccessContext
checkNiceModule c a r n bs ds
  = checkTypedBindings True c bs
  >>= \c' -> checkDeclarations (c <> c') ds
  >>= \c'' -> pure (toContext c'')
  >>= \c''' -> maybe (pure (fromContext a c''')) (checkModuleName c''' a r) n

checkNiceModuleMacro
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> Access
  -> Range
  -> N.Name
  -> ModuleApplication
  -> OpenShortHand
  -> ImportDirective
  -> m AccessContext
checkNiceModuleMacro c a _ a' (SectionApp r bs e) o i
  = checkSectionApp c a r a' bs (parseSectionApp e) o i
checkNiceModuleMacro _ _ r _ (RecordModuleInstance _ _) _ _
  = throwError (ErrorUnsupported UnsupportedMacro r)

checkSectionApp
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> Access
  -> Range
  -> N.Name
  -> [TypedBinding]
  -> Maybe (N.QName, [Expr])
  -> OpenShortHand
  -> ImportDirective
  -> m AccessContext
checkSectionApp _ _ r _ _ Nothing _ _
  = throwError (ErrorInternal (ErrorMacro r))
checkSectionApp c _ r (N.NoName _ _) [] (Just (n, es)) DoOpen i
  = liftMaybe (ErrorInternal (ErrorName (getRange n))) (fromQName n)
  >>= \n' -> liftLookup r n' (accessContextLookupModule n' c)
  >>= \(C.Module rs c') -> modifyDelete rs
  >> checkExprs c es
  >> checkImportDirective Open r n' c' i
  >>= pure . fromContext (importDirectiveAccess i)
checkSectionApp c a r a' bs (Just (n, es)) DontOpen i
  = liftMaybe (ErrorInternal (ErrorName (getRange n))) (fromQName n)
  >>= \n' -> liftMaybe (ErrorInternal (ErrorName (getRange a'))) (fromName a')
  >>= \a'' -> liftLookup r n' (accessContextLookupModule n' c)
  >>= \(C.Module rs c') -> modifyDelete rs
  >> checkTypedBindings True c bs
  >>= \c'' -> checkExprs (c <> c'') es
  >> checkImportDirective Module r (QName a'') c' i
  >>= \c''' -> checkModuleName c''' a r a''
checkSectionApp c a r a' bs (Just (n, es)) DoOpen i
  = liftMaybe (ErrorInternal (ErrorName (getRange n))) (fromQName n)
  >>= \n' -> liftMaybe (ErrorInternal (ErrorName (getRange a'))) (fromName a')
  >>= \a'' -> liftLookup r n' (accessContextLookupModule n' c)
  >>= \(C.Module rs c') -> modifyDelete rs
  >> checkTypedBindings True c bs
  >>= \c'' -> checkExprs (c <> c'') es
  >> checkImportDirective Module r (QName a'') c' i
  >>= \c''' -> checkModuleName c''' a r a''
  >>= \c'''' -> pure (c'''' <> fromContext (importDirectiveAccess i) c''')

parseSectionApp
  :: Expr
  -> Maybe (N.QName, [Expr])
parseSectionApp (Ident n)
  = Just (n, [])
parseSectionApp (RawApp _ (List2 (Ident n) e es))
  = Just (n, (e : es))
parseSectionApp _
  = Nothing

-- ## Imports

data DirectiveType where

  Import
    :: DirectiveType

  Module
    :: DirectiveType

  Open
    :: DirectiveType

  deriving Show

directiveStatement
  :: DirectiveType
  -> Maybe RangeType
directiveStatement Import
  = Just RangeImport
directiveStatement Module
  = Nothing
directiveStatement Open
  = Just RangeOpen

directiveItem
  :: DirectiveType
  -> RangeType
directiveItem Import
  = RangeImportItem
directiveItem Module
  = RangeModuleItem
directiveItem Open
  = RangeOpenItem

checkImportDirective
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => DirectiveType
  -> Range
  -> QName
  -> Context
  -> ImportDirective
  -> m Context
checkImportDirective dt r n c (ImportDirective _ UseEverything hs rs _)
  = traverse (\t -> modifyInsert r (RangeNamed t n)) (directiveStatement dt)
  >> modifyHidings c (hs <> (renFrom <$> rs))
  >>= \c' -> checkRenamings dt c rs
  >>= \c'' -> contextInsertRangeAll r (c' <> c'')
checkImportDirective dt r n c (ImportDirective _ (Using ns) _ rs _)
  = traverse (\t -> modifyInsert r (RangeNamed t n)) (directiveStatement dt)
  >> checkImportedNames dt c ns
  >>= \c' -> checkRenamings dt c rs
  >>= \c'' -> contextInsertRangeAll r (c' <> c'')

checkRenaming
  :: MonadReader Environment m
  => MonadState State m
  => MonadError Error m
  => DirectiveType
  -> Context
  -> Renaming
  -> m Context
checkRenaming dt c r@(Renaming n t _ _)
  = checkImportedNamePair dt c (getRange r, n, t)

checkRenamings
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => DirectiveType
  -> Context
  -> [Renaming]
  -> m Context
checkRenamings dt
  = checkSequence (checkRenaming dt)

checkImportedName
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => DirectiveType
  -> Context
  -> ImportedName
  -> m Context
checkImportedName dt c n
  = checkImportedNamePair dt c (getRange n, n, n)

checkImportedNamePair
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => DirectiveType
  -> Context
  -> (Range, ImportedName, ImportedName)
  -> m Context
checkImportedNamePair dt c (_, ImportedName n, ImportedName t)
  = liftMaybe (ErrorInternal (ErrorName (getRange n))) (fromName n)
  >>= \n' -> liftMaybe (ErrorInternal (ErrorName (getRange t)))
    (fromNameRange t)
  >>= \(r, t') -> modifyInsert r (RangeNamed (directiveItem dt) (QName t'))
  >> pure (maybe mempty (contextItem t') (contextLookupItem (QName n') c)
    <> maybe mempty (contextModule t') (contextLookupModule (QName n') c))
  >>= contextInsertRangeAll r
checkImportedNamePair dt c (_, ImportedModule n, ImportedModule t)
  = liftMaybe (ErrorInternal (ErrorName (getRange n))) (fromName n)
  >>= \n' -> liftMaybe (ErrorInternal (ErrorName (getRange t)))
    (fromNameRange t)
  >>= \(r, t') -> modifyInsert r (RangeNamed (directiveItem dt) (QName t'))
  >> pure (maybe mempty (contextModule t') (contextLookupModule (QName n') c))
  >>= contextInsertRangeAll r
checkImportedNamePair _ _ (r, _, _)
  = throwError (ErrorInternal (ErrorRenaming r))

checkImportedNames
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => DirectiveType
  -> Context
  -> [ImportedName]
  -> m Context
checkImportedNames dt
  = checkSequence (checkImportedName dt)

modifyHiding
  :: MonadError Error m
  => Context
  -> ImportedName
  -> m Context
modifyHiding c (ImportedName n)
  = liftMaybe (ErrorInternal (ErrorName (getRange n))) (fromName n)
  >>= pure . flip contextDelete c
modifyHiding c (ImportedModule n)
  = liftMaybe (ErrorInternal (ErrorName (getRange n))) (fromName n)
  >>= pure . flip contextDeleteModule c

modifyHidings
  :: MonadError Error m
  => Context
  -> [ImportedName]
  -> m Context
modifyHidings
  = foldM modifyHiding

touchModule
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => AccessContext
  -> Range
  -> QName
  -> m ()
touchModule c r n
  = touchModuleWith (accessContextLookupModule n c) r n

touchModuleWith
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => Either LookupError C.Module
  -> Range
  -> QName
  -> m ()
touchModuleWith (Left LookupNotFound) _ _
  = pure ()
touchModuleWith (Left LookupAmbiguous) r n
  = throwError (ErrorAmbiguous r n)
touchModuleWith (Right m) _ _
  = modifyDelete (moduleRanges m)

touchContext
  :: MonadReader Environment m
  => MonadState State m
  => Context
  -> m ()
touchContext c
  = modifyDelete (contextRanges c)

touchAccessContext
  :: MonadReader Environment m
  => MonadState State m
  => AccessContext
  -> m ()
touchAccessContext c
  = modifyDelete (accessContextRanges c)

importDirectiveAccess
  :: ImportDirective
  -> Access
importDirectiveAccess (ImportDirective _ _ _ _ Nothing)
  = Private
importDirectiveAccess (ImportDirective _ _ _ _ (Just _))
  = Public

-- ## Modules

checkModule
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => QName
  -> Module
  -> m Context
checkModule n (Mod _ ds) = do
  local
    <- askLocal
  _
    <- modifyBlock n
  context
    <- toContext <$> checkDeclarationsTop mempty ds
  _
    <- modifyCheck n context
  _
    <- when local (touchContext context)
  pure context

-- ## Files

checkFile
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Range
  -> QName
  -> m Context
checkFile r n
  = getModule n
  >>= checkFileWith r n

checkFileWith
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Range
  -> QName
  -> Maybe ModuleState
  -> m Context
checkFileWith r n Nothing
  = askRoot
  >>= \p -> liftMaybe (ErrorInternal (ErrorName r)) (qNamePath n)
  >>= \p' -> pure (p </> p')
  >>= \p'' -> liftIO (doesFileExist p'')
  >>= \b -> checkFileWith' r n (bool Nothing (Just p'') b)
checkFileWith r n (Just Blocked)
  = throwError (ErrorCyclic r n)
checkFileWith _ _ (Just (Checked c))
  = pure c

checkFileWith'
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Range
  -> QName
  -> Maybe FilePath
  -> m Context
checkFileWith' r n Nothing
  = checkFileExternal r n
checkFileWith' _ n (Just p)
  = checkFilePath n p

checkFileExternal
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Range
  -> QName
  -> m Context
checkFileExternal r n = do
  includes
    <- askIncludes
  sources
    <- getSources
  rawModuleName
    <- pure (rawTopLevelModuleNameForQName (toQName n))
  moduleName
    <- topLevelModuleName rawModuleName
  (pathEither, sources')
    <- liftIO (findFile'' includes moduleName sources)
  path
    <- liftEither (mapLeft (ErrorFind r n) pathEither)
  _
    <- modifySources sources'
  context
    <- localSkip (checkFilePath n (filePath (srcFilePath path)))
  pure context

checkFilePath
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => QName
  -> FilePath
  -> m Context
checkFilePath n p
  = readModule p
  >>= checkModule n

checkFileTop
  :: MonadError Error m
  => MonadIO m
  => Mode
  -> UnusedOptions
  -> FilePath
  -- ^ The file to check.
  -> m (FilePath, State)
  -- ^ The project root, along with the final state.
checkFileTop m opts p
  = runStateT (checkFileTop' m opts p) stateEmpty

checkFileTop'
  :: MonadError Error m
  => MonadState State m
  => MonadIO m
  => Mode
  -> UnusedOptions
  -> FilePath
  -- ^ The file to check.
  -> m FilePath
  -- ^ The project root.
checkFileTop' m opts p = do
  module'
    <- readModule p
  rawModuleName
    <- pure (rawTopLevelModuleNameForModule module')
  moduleName
    <- topLevelModuleName rawModuleName
  moduleQName
    <- pure (fromModuleName rawModuleName)
  absolutePath
    <- pure (projectRoot (mkAbsolute p) moduleName)
  rootPath
    <- pure (filePath absolutePath)
  includesEither
    <- liftIO (runTCMTop (setOptions opts >> getIncludeDirs))
  includes
    <- liftEither (mapLeft (const ErrorInclude) includesEither)
  env
    <- pure (Environment m rootPath includes)
  _
    <- runReaderT (checkModule moduleQName module') env
  pure rootPath

readModule
  :: MonadError Error m
  => MonadIO m
  => FilePath
  -> m Module
readModule p = do
  exists
    <- liftIO (doesFileExist p)
  _
    <- unless exists (throwError (ErrorFile p))
  rangeFile
    <- pure (RangeFile (mkAbsolute p) Nothing)
  contents
    <- liftIO (readFile p)
  (parseResult, _)
    <- liftIO (runPMIO (parseFile moduleParser rangeFile contents))
  ((module', _), _)
    <- liftEither (mapLeft ErrorParse parseResult)
  pure module'

topLevelModuleName
  :: MonadState State m
  => RawTopLevelModuleName
  -> m TopLevelModuleName
topLevelModuleName n
  = unsafeTopLevelModuleName n <$> getHash

-- ## Paths

-- Look for unvisited modules.
checkPath
  :: Set QName
  -- ^ Visited modules.
  -> FilePath
  -- ^ A path to ignore.
  -> FilePath
  -- ^ The project root path.
  -> IO [FilePath]
checkPath ns i r
  = Set.toAscList <$> checkPath' ns i r r

-- Look for unvisited modules at the given path.
checkPath'
  :: Set QName
  -- ^ Visited modules.
  -> FilePath
  -- ^ A path to ignore.
  -> FilePath
  -- ^ The project root path.
  -> FilePath
  -- ^ The path at which to look.
  -> IO (Set FilePath)
checkPath' ns i r p
  = liftIO (doesDirectoryExist p)
  >>= bool (pure (checkPathFile ns i r p)) (checkPathDirectory ns i r p)

checkPathFile
  :: Set QName
  -- ^ Visited modules.
  -> FilePath
  -- ^ A path to ignore.
  -> FilePath
  -- ^ The project root path.
  -> FilePath
  -- ^ The path at which to look.
  -> Set FilePath
checkPathFile _ i _ p | i == p
  = mempty
checkPathFile ns _ r p
  = maybe mempty (bool (Set.singleton p) mempty . flip Set.member ns)
  $ pathQName r p

checkPathDirectory
  :: Set QName
  -- ^ Visited modules.
  -> FilePath
  -- ^ A path to ignore.
  -> FilePath
  -- ^ The project root path.
  -> FilePath
  -- ^ The path at which to look.
  -> IO (Set FilePath)
checkPathDirectory ns i r p
  = fmap (p </>) <$> liftIO (listDirectory p)
  >>= traverse (checkPath' ns i r)
  >>= pure . mconcat

-- ## Main

-- | Check an Agda file and its dependencies for unused code, excluding public
-- items that could be imported elsewhere.
checkUnused
  :: UnusedOptions
  -- ^ Options to use.
  -> FilePath
  -- ^ Absolute path of the file to check.
  -> IO (Either Error UnusedItems)
checkUnused
  = checkUnusedWith Local

-- | Check an Agda file and its dependencies for unused code, using the
-- specified check mode.
checkUnusedWith
  :: Mode
  -- ^ The check mode to use.
  -> UnusedOptions
  -- ^ Options to use.
  -> FilePath
  -- ^ Absolute path of the file to check.
  -> IO (Either Error UnusedItems)
checkUnusedWith m opts
  = runExceptT
  . fmap (UnusedItems . stateItems . snd)
  . checkFileTop m opts

-- | Check an Agda file and its dependencies for unused code, including public
-- items in dependencies, as well as files.
--
-- The given file should consist only of import statements; it serves as a
-- full description of the public interface of the project.
checkUnusedGlobal
  :: UnusedOptions
  -- ^ Options to use.
  -> FilePath
  -- ^ Absolute path of the file to check.
  -> IO (Either Error Unused)
checkUnusedGlobal opts p
  = runExceptT (checkUnusedGlobal' opts p)

checkUnusedGlobal'
  :: UnusedOptions
  -> FilePath
  -> ExceptT Error IO Unused
checkUnusedGlobal' opts p = do
  (rootPath, state)
    <- checkFileTop GlobalMain opts p
  files
    <- liftIO (checkPath (stateModules state) p rootPath)
  items 
    <- pure (UnusedItems (filter (not . inFile p) (stateItems state)))
  unused
    <- pure (Unused files items)
  pure unused

setOptions
  :: UnusedOptions
  -> TCM ()
setOptions opts
  = setCommandLineOptions
  $ defaultOptions
  { optIncludePaths
    = unusedOptionsInclude opts
  , optLibraries
    = T.unpack <$> unusedOptionsLibraries opts
  , optOverrideLibrariesFile
    = unusedOptionsLibrariesFile opts
  , optDefaultLibs
    = unusedOptionsUseDefaultLibraries opts
  , optUseLibs
    = unusedOptionsUseLibraries opts
  }

inFile
  :: FilePath
  -> (Range, RangeInfo)
  -> Bool
inFile p (r, _)
  = rangePath r == Just p

