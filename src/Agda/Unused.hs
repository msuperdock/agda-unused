module Agda.Unused
  ( checkUnused
  ) where

import Agda.Unused.Monad.Context
  (AccessContext, Context, (\\), accessContextClear, accessContextCons,
    accessContextCons', accessContextDefiningMay, accessContextExport,
    accessContextImport, accessContextLookup, accessContextLookupModule,
    accessContextLookupName, accessContextOperators, accessContextOperatorsP,
    accessContextPrivate, accessContextSingleton,
    accessContextSingletonConstructor, accessContextUnion, contextCons,
    contextDelete, contextDeleteModule, contextInsert, contextInsertAll,
    contextInsertModule, contextLookup, contextLookupItem, contextLookupModule,
    contextRanges, contextRename, contextRenameModule, contextSingleton,
    fromContext)
import Agda.Unused.Monad.Error
  (Error (..), InternalError (..), LookupError (..), UnexpectedError (..),
    UnsupportedError (..), liftLookup)
import Agda.Unused.Monad.Reader
  (Environment (..), askBuiltin, askRoot, localBuiltin)
import Agda.Unused.Monad.State
  (ModuleState (..), State (..), modifyDelete, modifyInsert, stateBlock,
    stateCheck, stateEmpty, stateLookup)
import Agda.Unused.Types.Access
  (Access (..), access, fromAccess)
import Agda.Unused.Types.Name
  (Name (..), QName (..), isBuiltin, fromAsName, fromName, fromNameRange,
    fromQName, fromQNameRange, matchNames, nameIds, qNamePath)
import Agda.Unused.Types.Range
  (Range' (..), RangeInfo (..), RangeType (..))
import Agda.Unused.Types.Root
  (Root (..))
import Agda.Unused.Utils
  (liftMaybe, mapLeft)

import Agda.Syntax.Common
  (Arg (..), ImportDirective' (..), ImportedName' (..), Named (..), Renaming'
    (..), RewriteEqn' (..), Using' (..), namedThing, unArg, whThing)
import qualified Agda.Syntax.Common
  as C
import Agda.Syntax.Concrete
  (Binder, Binder' (..), BoundName (..), Declaration, DoStmt (..), Expr (..),
    FieldAssignment, FieldAssignment' (..), ImportDirective, ImportedName,
    LamBinding, LamBinding' (..), LamClause (..), LHS (..), Module,
    ModuleApplication (..), ModuleAssignment (..), OpenShortHand (..), Pattern
    (..), RecordAssignment, Renaming, RewriteEqn, RHS, RHS' (..), TypedBinding,
    TypedBinding' (..), WhereClause, WhereClause' (..), _exprFieldA)
import Agda.Syntax.Concrete.Definitions
  (Clause (..), NiceDeclaration (..), niceDeclarations, runNice)
import Agda.Syntax.Concrete.Fixity
  (DoWarn (..), fixitiesAndPolarities)
import Agda.Syntax.Concrete.Name
  (NamePart (..))
import qualified Agda.Syntax.Concrete.Name
  as N
import Agda.Syntax.Parser
  (moduleParser, parseFile, runPMIO)
import Agda.Syntax.Position
  (Range, getRange)
import Agda.Utils.FileName
  (AbsolutePath (..))
import Control.Monad
  (foldM, void)
import Control.Monad.Except
  (MonadError, liftEither, runExceptT, throwError)
import Control.Monad.IO.Class
  (MonadIO, liftIO)
import Control.Monad.Reader
  (MonadReader, runReaderT)
import Control.Monad.State
  (MonadState, gets, modify, runStateT)
import Data.Bool
  (bool)
import Data.Map.Strict
  (Map)
import Data.Maybe
  (catMaybes)
import qualified Data.Text
  as T
import System.Directory
  (doesFileExist)
import System.FilePath
  ((</>))
import Prelude hiding
  ((<>))
import qualified Prelude
  as P


-- Change associativity to left, for consistency with (\\).
infixl 6 <>

(<>)
  :: Semigroup a
  => a
  -> a
  -> a
(<>)
  = (P.<>)

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

checkSequence_
  :: Monad m
  => (a -> b -> m ())
  -> a
  -> [b]
  -> m ()
checkSequence_ f x ys
  = void (sequence (f x <$> ys))

checkFold
  :: Monad m
  => Monoid a
  => (a -> b -> m a)
  -> a
  -> [b]
  -> m a
checkFold
  = checkFoldWith mempty (<>)

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
  -- ^ Whether to add new names to state.
  -> Access
  -> RangeType
  -> Range
  -> Name
  -> m AccessContext
checkName _ _ _ NoRange _
  = pure mempty
checkName _ _ _ _ (Name [Hole])
  = pure mempty
checkName b a t r@(Range _ _) n
  = bool (pure ()) (modifyInsert r (RangeInfo t (QName n))) b
  >> accessContextSingleton n a [r]

checkName'
  :: MonadReader Environment m
  => MonadState State m
  => Bool
  -- ^ Whether to add new names to state.
  -> Access
  -> RangeType
  -> N.Name
  -> m AccessContext
checkName' b a t n
  = maybe (pure mempty) (uncurry (checkName b a t)) (fromNameRange n)

checkNames'
  :: MonadReader Environment m
  => MonadState State m
  => Access
  -> RangeType
  -> [N.Name]
  -> m AccessContext
checkNames' a
  = checkSequence (checkName' True a)

checkQNameP
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => Maybe Name
  -- ^ A name to avoid checking.
  -> AccessContext
  -> Range
  -> QName
  -> m AccessContext
checkQNameP (Just (Name ps)) _ _ (QName (Name [p])) | elem p ps
  = pure mempty
checkQNameP _ c r n
  = checkQNamePWith (accessContextLookup n c) c r n

checkQNamePWith
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => Either LookupError [Range]
  -> AccessContext
  -> Range
  -> QName
  -> m AccessContext
checkQNamePWith (Left LookupNotFound) _ r n
  = checkQName Public RangeVariable r n
checkQNamePWith _ c r n
  = touchQName c r n >> pure mempty

checkQNameP'
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => Maybe Name
  -- ^ A name to avoid checking.
  -> AccessContext
  -> N.QName
  -> m AccessContext
checkQNameP' m c n
  = maybe (pure mempty) (uncurry (checkQNameP m c)) (fromQNameRange n)

checkQName
  :: MonadReader Environment m
  => MonadState State m
  => Access
  -> RangeType
  -> Range
  -> QName
  -> m AccessContext
checkQName a t r (QName n)
  = checkName True a t r n
checkQName _ _ _ (Qual _ _)
  = pure mempty

touchName
  :: MonadReader Environment m
  => MonadState State m
  => AccessContext
  -> Name
  -> m ()
touchName c n
  = modifyDelete (accessContextLookupName n c)

touchNames
  :: MonadReader Environment m
  => MonadState State m
  => AccessContext
  -> [Name]
  -> m ()
touchNames
  = checkSequence_ touchName

touchQName
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => AccessContext
  -> Range
  -> QName
  -> m ()
touchQName c r n
  = askBuiltin >>= \b -> touchQNameWith b (accessContextLookup n c) r n

touchQNameWith
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => Bool
  -- ^ Whether we are in a builtin module.
  -> Either LookupError [Range]
  -> Range
  -> QName
  -> m ()
touchQNameWith False (Left LookupNotFound) _ _
  = pure ()
touchQNameWith False (Left LookupAmbiguous) r n
  = throwError (ErrorAmbiguous r n)
touchQNameWith False (Right rs) _ _
  = modifyDelete rs
touchQNameWith True _ _ _
  = pure ()

touchQNameContext
  :: MonadReader Environment m
  => MonadState State m
  => Context
  -> QName
  -> m ()
touchQNameContext c n
  = maybe (pure ()) modifyDelete (contextLookup n c)

touchQNamesContext
  :: MonadReader Environment m
  => MonadState State m
  => Context
  -> [QName]
  -> m ()
touchQNamesContext c
  = void . traverse (touchQNameContext c)

touchQName'
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => AccessContext
  -> N.QName
  -> m ()
touchQName' c n
  = maybe (pure ()) (uncurry (touchQName c)) (fromQNameRange n)

-- ## Bindings

checkBinder
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Bool
  -- ^ Whether to add new names to state.
  -> AccessContext
  -> Binder
  -> m AccessContext
checkBinder b c (Binder p (BName n _ _))
  = checkName' b Public RangeVariable n
  >>= \c' -> checkPatternMay c p
  >>= \c'' -> pure (c' <> c'')

checkBinders
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Bool
  -- ^ Whether to add new names to state.
  -> AccessContext
  -> [Binder]
  -> m AccessContext
checkBinders b
  = checkSequence (checkBinder b)

checkLamBinding
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Bool
  -- ^ Whether to add new names to state.
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
  -- ^ Whether to add new names to state.
  -> AccessContext
  -> [LamBinding]
  -> m AccessContext
checkLamBindings b
  = checkFold (checkLamBinding b)

checkTypedBinding
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Bool
  -- ^ Whether to add new names to state.
  -> AccessContext
  -> TypedBinding
  -> m AccessContext
checkTypedBinding b c (TBind _ bs e)
  = checkExpr c e >> checkBinders b c (namedThing . unArg <$> bs)
checkTypedBinding _ c (TLet _ ds)
  = checkDeclarations c ds

checkTypedBindings
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> [TypedBinding]
  -> m AccessContext
checkTypedBindings
  = checkFold (checkTypedBinding True)

-- ## Patterns

checkPattern
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Maybe Name
  -- ^ A name to avoid checking.
  -> AccessContext
  -> Pattern
  -> m AccessContext
checkPattern m c (IdentP n)
  = checkQNameP' m c n
checkPattern _ _ (QuoteP _)
  = pure mempty
checkPattern m c (AppP p (Arg _ (Named _ p')))
  = (<>) <$> checkPattern m c p
    <*> checkPattern m c p'
checkPattern m c (RawAppP _ ps)
  = checkRawAppP m c ps
checkPattern _ _ (OpAppP r _ _ _)
  = throwError (ErrorInternal (ErrorUnexpected UnexpectedOpAppP) r)
checkPattern m c (HiddenP _ (Named _ p))
  = checkPattern Nothing (c \\ m) p
checkPattern m c (InstanceP _ (Named _ p))
  = checkPattern Nothing (c \\ m) p
checkPattern m c (ParenP _ p)
  = checkPattern m c p
checkPattern _ _ (WildP _)
  = pure mempty
checkPattern _ _ (AbsurdP _)
  = pure mempty
checkPattern m c (AsP _ n p)
  = (<>) <$> checkName' True Public RangeVariable n
    <*> checkPattern Nothing (c \\ m) p
checkPattern m c (DotP _ e)
  = checkExpr (c \\ m) e >> pure mempty
checkPattern _ _ (LitP _)
  = pure mempty
checkPattern m c (RecP _ as)
  = checkPatterns Nothing (c \\ m) (_exprFieldA <$> as)
checkPattern m c (EqualP _ es)
  = checkExprPairs (c \\ m) es >> pure mempty
checkPattern _ _ (EllipsisP _)
  = pure mempty
checkPattern m c (WithP _ p)
  = checkPattern Nothing (c \\ m) p

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
  = checkPattern Nothing c p

checkPatterns
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Maybe Name
  -> AccessContext
  -> [Pattern]
  -> m AccessContext
checkPatterns m
  = checkSequence (checkPattern m)

checkRawAppP
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Maybe Name
  -- ^ A name to avoid checking.
  -> AccessContext
  -> [Pattern]
  -> m AccessContext
checkRawAppP m c ps
  = pure (matchNames (patternNames ps) (accessContextOperatorsP c))
  >>= \ns -> touchNames (accessContextDefiningMay m c) ns
  >> checkPatterns m c (patternDelete ns ps)

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
patternName (IdentP (N.QName (N.Name _ _ [Id n])))
  = Just n
patternName _
  = Nothing

patternNames
  :: [Pattern]
  -> [String]
patternNames
  = catMaybes
  . fmap patternName

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
checkExpr _ (Lit _)
  = pure ()
checkExpr _ (QuestionMark _ _)
  = pure ()
checkExpr _ (Underscore _ _)
  = pure ()
checkExpr c (RawApp _ es)
  = checkRawApp c es
checkExpr c (App _ e (Arg _ (Named _ e')))
  = checkExpr c e >> checkExpr c e'
checkExpr _ (OpApp r _ _ _)
  = throwError (ErrorInternal (ErrorUnexpected UnexpectedOpApp) r)
checkExpr c (WithApp _ e es)
  = checkExpr c e >> checkExprs c es
checkExpr c (HiddenArg _ (Named _ e))
  = checkExpr c e
checkExpr c (InstanceArg _ (Named _ e))
  = checkExpr c e
checkExpr c (Lam _ bs e)
  = checkLamBindings True c bs >>= \c' -> checkExpr (c <> c') e
checkExpr _ (AbsurdLam _ _)
  = pure ()
checkExpr c (ExtendedLam _ ls)
  = checkLamClauses_ c ls
checkExpr c (Fun _ (Arg _ e) e')
  = checkExpr c e >> checkExpr c e'
checkExpr c (Pi bs e)
  = checkTypedBindings c bs >>= \c' -> checkExpr (c <> c') e
checkExpr _ (Set _)
  = pure ()
checkExpr _ (Prop _)
  = pure ()
checkExpr _ (SetN _ _)
  = pure ()
checkExpr _ (PropN _ _)
  = pure ()
checkExpr c (Rec _ rs)
  = checkRecordAssignments c rs
checkExpr c (RecUpdate _ e fs)
  = checkExpr c e >> checkFieldAssignments c fs
checkExpr c (Let _ ds e)
  = checkDeclarations c ds >>= \c' -> maybe (pure ()) (checkExpr (c <> c')) e
checkExpr c (Paren _ e)
  = checkExpr c e
checkExpr c (IdiomBrackets _ es)
  = checkExprs c es
checkExpr c (DoBlock _ ss)
  = void (checkDoStmts c ss)
checkExpr _ (Absurd r)
  = throwError (ErrorInternal (ErrorUnexpected UnexpectedAbsurd) r)
checkExpr _ (As r _ _)
  = throwError (ErrorInternal (ErrorUnexpected UnexpectedAs) r)
checkExpr c (Dot _ e)
  = checkExpr c e
checkExpr c (DoubleDot _ e)
  = checkExpr c e
checkExpr _ e@(ETel _)
  = throwError (ErrorInternal (ErrorUnexpected UnexpectedETel) (getRange e))
checkExpr _ (Quote _)
  = pure ()
checkExpr _ (QuoteTerm _)
  = pure ()
checkExpr c (Tactic _ e)
  = checkExpr c e
checkExpr _ (Unquote _)
  = pure ()
checkExpr _ e@(DontCare _)
  = throwError (ErrorInternal (ErrorUnexpected UnexpectedDontCare) (getRange e))
checkExpr _ (Equal r _ _)
  = throwError (ErrorInternal (ErrorUnexpected UnexpectedEqual) r)
checkExpr _ (Ellipsis r)
  = throwError (ErrorInternal (ErrorUnexpected UnexpectedEllipsis) r)
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
  = checkSequence_ checkExpr

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
  = checkSequence_ checkExprPair

checkRawApp
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> [Expr]
  -> m ()
checkRawApp c es
  = touchNames c (matchNames (exprNames es) (accessContextOperators c))
  >> checkExprs c es

exprName
  :: Expr
  -> Maybe String
exprName (Ident (N.QName (N.Name _ _ [Id n])))
  = Just n
exprName _
  = Nothing

exprNames
  :: [Expr]
  -> [String]
exprNames
  = catMaybes
  . fmap exprName

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
  = checkSequence_ checkRecordAssignment

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
  = checkSequence_ checkFieldAssignment

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
  >> maybe (pure ()) (touchModule c (getRange a)) (fromQName n)

-- ## Definitions

checkLHS
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Maybe Name
  -- ^ A name to avoid checking.
  -> AccessContext
  -> LHS
  -> m AccessContext
checkLHS m c (LHS p rs ws _)
  = checkPattern m c p
  >>= \c' -> checkRewriteEqns (c <> c') rs
  >>= \c'' -> checkExprs (c <> c' <> c'') (whThing <$> ws)
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
  = checkLHS (fromName n) c l
  >>= \c' -> checkWhereClause (c <> c' \\ fromName n) w
  >>= \(m, c'') -> checkRHS (c <> c' \\ fromName n <> c'') r
  >> checkClauses (c <> c' <> c'') cs
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
checkLamClause c (LamClause l r _ _)
  = checkLHS Nothing c l
  >>= \c' -> checkRHS (c <> c') r
  >> pure c'

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

-- Do not propogate context from one clause to the next.
checkLamClauses_
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> [LamClause]
  -> m ()
checkLamClauses_
  = checkSequence_ checkLamClause_

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
checkWhereClause c (AnyWhere ds)
  = checkDeclarations c ds
  >>= \c' -> pure (mempty, c')
checkWhereClause c (SomeWhere n a ds)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromName n)
  >>= \n' -> checkDeclarations c ds
  >>= \c' -> pure (accessContextCons' n' (fromAccess a) c', c')

checkRewriteEqn
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> RewriteEqn
  -> m AccessContext
checkRewriteEqn c (Rewrite rs)
  = checkExprs c (snd <$> rs) >> pure mempty
checkRewriteEqn c (Invert _ ws)
  = checkIrrefutableWiths c ws

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
  = checkExpr c e >> checkPattern Nothing c p

checkIrrefutableWiths
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> [(Pattern, Expr)]
  -> m AccessContext
checkIrrefutableWiths
  = checkFold checkIrrefutableWith

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
  >> checkPattern Nothing (c <> c') p
  >>= \c'' -> pure (c' <> c'')
checkDoStmt c (DoThen e)
  = checkExpr c e >> pure mempty
checkDoStmt c (DoLet _ ds)
  = checkDeclarations c ds

checkDoStmts
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> [DoStmt]
  -> m AccessContext
checkDoStmts
  = checkFold checkDoStmt

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

checkDeclarationsRecord
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Name
  -> [Range]
  -- ^ Ranges associated with parent record.
  -> AccessContext
  -> [Declaration]
  -> m AccessContext
checkDeclarationsRecord n rs
  = checkDeclarationsWith (checkNiceDeclarationsRecord n rs)

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
  => (AccessContext -> [NiceDeclaration] -> m AccessContext)
  -> AccessContext
  -> [Declaration]
  -> m AccessContext
checkDeclarationsWith f c ds = do
  (fixities, _)
    <- fixitiesAndPolarities NoWarn ds
  (niceDeclsEither, _) 
    <- pure (runNice (niceDeclarations fixities ds))
  niceDecls
    <- liftEither
      $ mapLeft ErrorDeclaration
      $ niceDeclsEither
  f c niceDecls

checkNiceDeclaration
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> NiceDeclaration
  -> m AccessContext

checkNiceDeclaration c (Axiom _ a _ _ _ n e)
  = checkExpr c e >> checkName' True (fromAccess a) RangeDefinition n
checkNiceDeclaration _ (NiceField r _ _ _ _ _ _)
  = throwError (ErrorInternal (ErrorUnexpected UnexpectedField) r)
checkNiceDeclaration c (PrimitiveFunction _ a _ n e)
  = checkExpr c e >> checkName' True (fromAccess a) RangeDefinition n
checkNiceDeclaration c (NiceMutual _ _ _ _ ds)
  = checkNiceDeclarations c ds
checkNiceDeclaration c (NiceModule _ a _ (N.QName n) bs ds)
  = checkNiceModule c (fromAccess a) (fromName n) bs ds
checkNiceDeclaration _ (NiceModule _ _ _ n@(N.Qual _ _) _ _)
  = throwError (ErrorInternal ErrorName (getRange n))
checkNiceDeclaration _ (NicePragma _ _)
  = pure mempty
checkNiceDeclaration c (NiceRecSig _ a _ _ _ n bs e)
  = checkNiceSig c a RangeRecord n bs e
checkNiceDeclaration c (NiceDataSig _ a _ _ _ n bs e)
  = checkNiceSig c a RangeData n bs e
checkNiceDeclaration _ (NiceFunClause r _ _ _ _ _ _)
  = throwError (ErrorInternal (ErrorUnexpected UnexpectedNiceFunClause) r)
checkNiceDeclaration c (FunSig _ a _ _ _ _ _ _ n e)
  = checkExpr c e >> checkName' True (fromAccess a) RangeDefinition n
checkNiceDeclaration c (FunDef _ _ _ _ _ _ _ cs)
  = checkClauses c cs >> pure mempty
checkNiceDeclaration c (NiceGeneralize _ _ _ _ _ e)
  = checkExpr c e >> pure mempty
checkNiceDeclaration _ (NiceUnquoteDecl r _ _ _ _ _ _ _)
  = throwError (ErrorUnsupported UnsupportedUnquote r)
checkNiceDeclaration _ (NiceUnquoteDef r _ _ _ _ _ _)
  = throwError (ErrorUnsupported UnsupportedUnquote r)

checkNiceDeclaration c
  (NiceModuleMacro r _ (N.NoName _ _)
    (SectionApp _ [] (RawApp _ (Ident n : es))) DoOpen i)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromQName n)
  >>= \n' -> liftLookup r n' (accessContextLookupModule n' c)
  >>= \c' -> checkExprs c es
  >> checkImportDirective Open r n' c' i
  >>= pure . fromContext (importDirectiveAccess i)
checkNiceDeclaration _
  (NiceModuleMacro r _ _ _ _ _)
  = throwError (ErrorUnsupported UnsupportedMacro r)

checkNiceDeclaration c (NiceOpen r n i)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromQName n)
  >>= \n' -> liftLookup r n' (accessContextLookupModule n' c)
  >>= \c' -> checkImportDirective Open r n' c' i
  >>= pure . fromContext (importDirectiveAccess i)

checkNiceDeclaration _ (NiceImport r n Nothing DontOpen i)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromQName n)
  >>= \n' -> checkFile (Just r) n'
  >>= \c' -> checkImportDirective Import r n' c' i
  >>= \c'' -> pure (accessContextImport n' c'')
checkNiceDeclaration _ (NiceImport r n Nothing DoOpen i)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromQName n)
  >>= \n' -> checkFile (Just r) n'
  >>= \c' -> checkImportDirective Import r n' c' i
  >>= \c'' -> pure (accessContextImport n' c'
    <> fromContext (importDirectiveAccess i) c'')
checkNiceDeclaration _ (NiceImport r n (Just a) DontOpen i)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromQName n)
  >>= \n' -> liftMaybe (ErrorInternal ErrorName (getRange a)) (fromAsName a)
  >>= \a' -> checkFile (Just r) n'
  >>= \c' -> checkImportDirective Import r n' c' i
  >>= \c'' -> pure (accessContextCons a' Public c'')
checkNiceDeclaration _ (NiceImport r n (Just a) DoOpen i)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromQName n)
  >>= \n' -> liftMaybe (ErrorInternal ErrorName (getRange a)) (fromAsName a)
  >>= \a' -> checkFile (Just r) n'
  >>= \c' -> checkImportDirective Import r n' c' i
  >>= \c'' -> pure (accessContextCons a' Public c''
    <> fromContext (importDirectiveAccess i) c'')

checkNiceDeclaration c (NiceDataDef _ _ _ _ _ n bs cs)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromName n)
  >>= \n' -> pure (accessContextLookupName n' c)
  >>= \rs -> checkLamBindings False c bs
  >>= \c' -> checkNiceConstructors rs (c <> c' \\ Just n') cs
  >>= \c'' -> pure (accessContextCons' n' Public c'' <> c'')

checkNiceDeclaration c (NiceRecDef _ _ _ _ _ n _ _ m bs ds)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromName n)
  >>= \n' -> pure (accessContextLookupName n' c)
  >>= \rs -> checkLamBindings False c bs
  >>= \c' -> checkNiceConstructorRecordMay rs (m >>= fromNameRange . fst)
  >>= \c'' -> checkDeclarationsRecord n' rs (c <> c') ds
  >>= \c''' -> pure (accessContextCons' n' Public (c'' <> c''') <> c'')

checkNiceDeclaration c (NicePatternSyn _ a n ns p)
  = checkNames' Public RangeVariable (unArg <$> ns)
  >>= \c' -> checkPattern Nothing (c <> c') p
  >> checkName' True (fromAccess a) RangePatternSynonym n

checkNiceDeclarationRecord
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Name
  -> [Range]
  -- ^ Ranges associated with parent record.
  -> AccessContext
  -> NiceDeclaration
  -> m AccessContext
checkNiceDeclarationRecord _ rs c d@(Axiom _ _ _ _ _ _ _)
  = modifyDelete rs >> checkNiceDeclaration c d
checkNiceDeclarationRecord _ rs c d@(PrimitiveFunction _ _ _ _ _)
  = modifyDelete rs >> checkNiceDeclaration c d
checkNiceDeclarationRecord n rs c (NiceMutual _ _ _ _ ds)
  = checkNiceDeclarationsRecord n rs c ds
checkNiceDeclarationRecord _ rs c d@(NiceModule _ _ _ _ _ _)
  = modifyDelete rs >> checkNiceDeclaration c d
checkNiceDeclarationRecord _ _ c d@(NiceModuleMacro _ _ _ _ _ _)
  = checkNiceDeclaration c d
checkNiceDeclarationRecord _ _ c d@(NiceOpen _ _ _)
  = checkNiceDeclaration c d
checkNiceDeclarationRecord _ _ c d@(NiceImport _ _ _ _ _)
  = checkNiceDeclaration c d
checkNiceDeclarationRecord _ _ c d@(NicePragma _ _)
  = checkNiceDeclaration c d
checkNiceDeclarationRecord _ rs c d@(NiceRecSig _ _ _ _ _ _ _ _)
  = modifyDelete rs >> checkNiceDeclaration c d
checkNiceDeclarationRecord _ rs c d@(NiceDataSig _ _ _ _ _ _ _ _)
  = modifyDelete rs >> checkNiceDeclaration c d
checkNiceDeclarationRecord _ _ c d@(NiceFunClause _ _ _ _ _ _ _)
  = checkNiceDeclaration c d
checkNiceDeclarationRecord _ rs c d@(FunSig _ _ _ _ _ _ _ _ _ _)
  = modifyDelete rs >> checkNiceDeclaration c d
checkNiceDeclarationRecord _ _ c d@(FunDef _ _ _ _ _ _ _ _)
  = checkNiceDeclaration c d
checkNiceDeclarationRecord _ _ c d@(NiceDataDef _ _ _ _ _ _ _ _)
  = checkNiceDeclaration c d
checkNiceDeclarationRecord _ _ c d@(NiceRecDef _ _ _ _ _ _ _ _ _ _ _)
  = checkNiceDeclaration c d
checkNiceDeclarationRecord _ _ c d@(NicePatternSyn _ _ _ _ _)
  = checkNiceDeclaration c d
checkNiceDeclarationRecord _ _ c d@(NiceGeneralize _ _ _ _ _ _)
  = checkNiceDeclaration c d
checkNiceDeclarationRecord _ _ c d@(NiceUnquoteDecl _ _ _ _ _ _ _ _)
  = checkNiceDeclaration c d
checkNiceDeclarationRecord _ _ c d@(NiceUnquoteDef _ _ _ _ _ _ _)
  = checkNiceDeclaration c d

checkNiceDeclarationRecord n rs c (NiceField _ a _ _ _ n' (Arg _ e))
  = checkExpr (c \\ Just n) e
  >> maybe
    (pure mempty)
    (\n'' -> accessContextSingleton n'' (fromAccess a) rs)
    (fromName n')

checkNiceDeclarations
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> [NiceDeclaration]
  -> m AccessContext
checkNiceDeclarations
  = checkFoldUnion checkNiceDeclaration

checkNiceDeclarationsRecord
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Name
  -> [Range]
  -- ^ Ranges associated with parent record.
  -> AccessContext
  -> [NiceDeclaration]
  -> m AccessContext
checkNiceDeclarationsRecord n rs
  = checkFoldUnion (checkNiceDeclarationRecord n rs)

checkNiceDeclarationsTop
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> [NiceDeclaration]
  -> m AccessContext
checkNiceDeclarationsTop _ []
  = pure mempty
checkNiceDeclarationsTop c (NiceModule _ a _ _ bs ds : _)
  = checkNiceModule c (fromAccess a) Nothing bs ds
checkNiceDeclarationsTop c (d : ds)
  = checkNiceDeclaration c d >>= \c' -> checkNiceDeclarations (c <> c') ds

checkNiceSig
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> C.Access
  -> RangeType
  -> N.Name
  -> [LamBinding]
  -> Expr
  -> m AccessContext
checkNiceSig c a t n bs e
  = checkLamBindings False c bs
  >>= \c' -> checkExpr (c <> c') e
  >> checkName' True (fromAccess a) t n

checkNiceConstructor
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => [Range]
  -- ^ Ranges associated with parent type.
  -> AccessContext
  -> NiceDeclaration
  -> m AccessContext
checkNiceConstructor rs c (Axiom _ a _ _ _ n e)
  = checkExpr c e
  >> maybe
    (pure mempty)
    (\n'' -> accessContextSingletonConstructor n'' (fromAccess a) rs)
    (fromName n)
checkNiceConstructor _ _ d
  = throwError (ErrorInternal ErrorConstructor (getRange d))

checkNiceConstructors
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => [Range]
  -- ^ Ranges associated with parent type.
  -> AccessContext
  -> [NiceDeclaration]
  -> m AccessContext
checkNiceConstructors rs
  = checkSequence (checkNiceConstructor rs)

checkNiceConstructorRecord
  :: MonadReader Environment m
  => MonadState State m
  => [Range]
  -- ^ Ranges associated with record type.
  -> Range
  -> Name
  -> m AccessContext
checkNiceConstructorRecord rs r n
  = modifyInsert r (RangeInfo RangeRecordConstructor (QName n))
  >> accessContextSingletonConstructor n Public (r : rs)

checkNiceConstructorRecordMay
  :: MonadReader Environment m
  => MonadState State m
  => [Range]
  -- ^ Ranges associated with record type.
  -> Maybe (Range, Name)
  -> m AccessContext
checkNiceConstructorRecordMay _ Nothing
  = pure mempty
checkNiceConstructorRecordMay rs (Just (r, n))
  = checkNiceConstructorRecord rs r n

checkNiceModule
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> Access
  -> Maybe Name
  -- ^ If Nothing, the module is anonymous.
  -> [TypedBinding]
  -> [Declaration]
  -> m AccessContext
checkNiceModule c a n bs ds
  = checkTypedBindings c bs
  >>= \c' -> checkDeclarations (c <> c') ds
  >>= pure . maybe
    (accessContextClear . access accessContextPrivate id a)
    (flip accessContextCons' a) n

-- ## Imports

data DirectiveType where

  Import
    :: DirectiveType

  Open
    :: DirectiveType

  deriving Show

directiveStatement
  :: DirectiveType
  -> RangeType
directiveStatement Import
  = RangeImport
directiveStatement Open
  = RangeOpen

directiveItem
  :: DirectiveType
  -> RangeType
directiveItem Import
  = RangeImportItem
directiveItem Open
  = RangeOpenItem

checkImportDirective
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => DirectiveType
  -> Range
  -- ^ The range of the import statement.
  -> QName
  -> Context
  -> ImportDirective
  -> m Context
checkImportDirective t r n c (ImportDirective _ UseEverything hs rs _)
  = modifyInsert r (RangeInfo (directiveStatement t) n)
  >> modifyHidings c hs
  >>= flip (modifyRenamings t) rs
  >>= contextInsertAll r
checkImportDirective t r n c (ImportDirective _ (Using ns) _ rs _)
  = modifyInsert r (RangeInfo (directiveStatement t) n)
  >> checkImportedNames t c ns
  >>= \c' -> checkRenamings t c rs
  >>= \c'' -> contextInsertAll r (c' <> c'')

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
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromName n)
  >>= \n' -> liftMaybe (ErrorInternal ErrorName (getRange t)) (fromNameRange t)
  >>= \(r, t') -> modifyInsert r (RangeInfo (directiveItem dt) (QName t'))
  >> contextInsertAll r
    (maybe mempty (contextSingleton t') (contextLookupItem (QName n') c)
      <> maybe mempty (contextCons t') (contextLookupModule (QName n') c))
checkImportedNamePair dt c (_, ImportedModule n, ImportedModule t)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromName n)
  >>= \n' -> liftMaybe (ErrorInternal ErrorName (getRange t)) (fromNameRange t)
  >>= \(r, t') -> modifyInsert r (RangeInfo (directiveItem dt) (QName t'))
  >> contextInsertAll r
    (maybe mempty (contextCons t') (contextLookupModule (QName n') c))
checkImportedNamePair _ _ (r, _, _)
  = throwError (ErrorInternal ErrorRenaming r)

checkImportedNames
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => DirectiveType
  -> Context
  -> [ImportedName]
  -> m Context
checkImportedNames t
  = checkSequence (checkImportedName t)

modifyHiding
  :: MonadError Error m
  => Context
  -> ImportedName
  -> m Context
modifyHiding c (ImportedName n)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromName n)
  >>= pure . flip contextDelete c
modifyHiding c (ImportedModule n)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromName n)
  >>= pure . flip contextDeleteModule c

modifyHidings
  :: MonadError Error m
  => Context
  -> [ImportedName]
  -> m Context
modifyHidings
  = foldM modifyHiding

modifyRenaming
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => DirectiveType
  -> Context
  -> Renaming
  -> m Context
modifyRenaming dt c (Renaming (ImportedName n) (ImportedName t) _ _)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromName n)
  >>= \n' -> liftMaybe (ErrorInternal ErrorName (getRange t)) (fromNameRange t)
  >>= \(r, t') -> modifyInsert r (RangeInfo (directiveItem dt) (QName n'))
  >> contextInsert n' r c
  >>= pure . contextRename n' t'
  >>= contextInsertModule n' r
  >>= pure . contextRenameModule n' t'
modifyRenaming dt c (Renaming (ImportedModule n) (ImportedModule t) _ _)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromName n)
  >>= \n' -> liftMaybe (ErrorInternal ErrorName (getRange t)) (fromNameRange t)
  >>= \(r, t') -> modifyInsert r (RangeInfo (directiveItem dt) (QName n'))
  >> contextInsertModule n' r c
  >>= pure . contextRenameModule n' t'
modifyRenaming _ _ r
  = throwError (ErrorInternal ErrorRenaming (getRange r))

modifyRenamings
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => DirectiveType
  -> Context
  -> [Renaming]
  -> m Context
modifyRenamings t
  = foldM (modifyRenaming t)

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
  => Either LookupError Context
  -> Range
  -> QName
  -> m ()
touchModuleWith (Left LookupNotFound) _ _
  = pure ()
touchModuleWith (Left LookupAmbiguous) r n
  = throwError (ErrorAmbiguous r n)
touchModuleWith (Right c) _ _
  = modifyDelete (contextRanges c)

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
  => Module
  -> m Context
checkModule (_, ds)
  = accessContextExport <$> checkDeclarationsTop mempty ds

-- ## Files

checkFile
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Maybe Range
  -> QName
  -> m Context
checkFile r n
  = gets (stateLookup n)
  >>= \s -> checkFileWith s r n

checkFileWith
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Maybe ModuleState
  -> Maybe Range
  -> QName
  -> m Context
checkFileWith Nothing r n | isBuiltin n
  = localBuiltin (checkFilePath r n ("data" </> qNamePath n))
checkFileWith Nothing r n
  = askRoot >>= \p -> checkFilePath r n (p </> qNamePath n)

checkFileWith (Just Blocked) r n
  = throwError (ErrorCyclic r n)
checkFileWith (Just (Checked c)) _ _
  = pure c

checkFilePath
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Maybe Range
  -> QName
  -> FilePath
  -> m Context
checkFilePath r n p = do
  _
    <- modify (stateBlock n)
  absolutePath
    <- pure (AbsolutePath (T.pack p))
  exists
    <- liftIO (doesFileExist p)
  _
    <- bool (throwError (ErrorFile r n p)) (pure ()) exists
  contents
    <- liftIO (readFile p)
  (parseResult, _)
    <- liftIO (runPMIO (parseFile moduleParser absolutePath contents))
  (module', _)
    <- liftEither (mapLeft ErrorParse parseResult)
  context
    <- checkModule module'
  _
    <- modify (stateCheck n context)
  pure context

-- ## Roots

checkRoot
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Root
  -> m ()
checkRoot (Root f ns)
  = checkFile Nothing f
  >>= flip touchQNamesContext ns

checkRoots
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => [Root]
  -> m ()
checkRoots
  = void . traverse checkRoot

-- ## Main

checkUnused
  :: FilePath
  -> [Root]
  -> IO (Either Error (Map Range RangeInfo))
checkUnused p
  = runExceptT
  . fmap stateUnused
  . fmap snd
  . flip runStateT stateEmpty
  . flip runReaderT (Environment False p)
  . checkRoots

