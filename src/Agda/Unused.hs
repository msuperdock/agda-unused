module Agda.Unused
  ( checkUnused
  ) where

import Agda.Unused.Monad.Error
  (Error(..), InternalError(..), UnexpectedError(..), UnsupportedError(..),
    liftLookup)
import Agda.Unused.Monad.Reader
  (Environment(..), askBuiltin, askRoot, localBuiltin)
import Agda.Unused.Monad.State
  (ModuleState(..), State(..), modifyDelete, modifyInsert, stateBlock,
    stateCheck, stateEmpty, stateLookup)
import Agda.Unused.Types.Access
  (Access(..), fromAccess)
import Agda.Unused.Types.Context
  (AccessContext, Context, LookupError(..), accessContextDefine,
    accessContextImport, accessContextItem, accessContextLookup,
    accessContextLookupDefining, accessContextLookupModule,
    accessContextLookupSpecial, accessContextMatch, accessContextModule,
    accessContextModule', accessContextUnion, contextDelete,
    contextDeleteModule, contextItem, contextLookup, contextLookupItem,
    contextLookupModule, contextModule, contextRanges, fromContext, item,
    itemConstructor, toContext)
import qualified Agda.Unused.Types.Context
  as C
import Agda.Unused.Types.Name
  (Name(..), QName(..), isBuiltin, fromAsName, fromName, fromNameRange,
    fromQName, fromQNameRange, nameIds, qNamePath)
import Agda.Unused.Types.Range
  (Range'(..), RangeInfo(..), RangeType(..))
import Agda.Unused.Types.Root
  (Root(..))
import Agda.Unused.Utils
  (liftMaybe, mapLeft)

import Agda.Syntax.Common
  (Arg(..), Fixity'(..), GenPart(..), ImportDirective'(..), ImportedName'(..),
    Named(..), Ranged(..), Renaming'(..), RewriteEqn'(..), Using'(..),
    namedThing, unArg, whThing)
import qualified Agda.Syntax.Common
  as Common
import Agda.Syntax.Concrete
  (Binder, Binder'(..), BoundName(..), Declaration, DoStmt(..), Expr(..),
    FieldAssignment, FieldAssignment'(..), ImportDirective, ImportedName,
    LamBinding, LamBinding'(..), LamClause(..), LHS(..), Module,
    ModuleApplication(..), ModuleAssignment(..), OpenShortHand(..), Pattern(..),
    RecordAssignment, Renaming, RewriteEqn, RHS, RHS'(..), TypedBinding,
    TypedBinding'(..), WhereClause, WhereClause'(..), _exprFieldA)
import Agda.Syntax.Concrete.Definitions
  (Clause(..), NiceDeclaration(..), niceDeclarations, runNice)
import Agda.Syntax.Concrete.Fixity
  (DoWarn(..), Fixities, fixitiesAndPolarities)
import Agda.Syntax.Concrete.Name
  (NameInScope(..), NamePart(..))
import qualified Agda.Syntax.Concrete.Name
  as N
import Agda.Syntax.Parser
  (moduleParser, parseFile, runPMIO)
import Agda.Syntax.Position
  (Range, getRange)
import Agda.Utils.FileName
  (AbsolutePath(..))
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
import qualified Data.Map.Strict
  as Map
import Data.Maybe
  (catMaybes)
import qualified Data.Text
  as T
import System.Directory
  (doesFileExist)
import System.FilePath
  ((</>))

-- ## Contexts

-- Do nothing if in builtin module.
contextInsertRange
  :: MonadReader Environment m
  => Name
  -> Range
  -> Context
  -> m Context
contextInsertRange n r c
  = askBuiltin >>= pure . bool (C.contextInsertRange n r c) c

-- Do nothing if in builtin module.
contextInsertRangeModule
  :: MonadReader Environment m
  => Name
  -> Range
  -> Context
  -> m Context
contextInsertRangeModule n r c
  = askBuiltin >>= pure . bool (C.contextInsertRangeModule n r c) c

-- Do nothing if in builtin module.
contextInsertRangeAll
  :: MonadReader Environment m
  => Range
  -> Context
  -> m Context
contextInsertRangeAll r c
  = askBuiltin >>= pure . bool (C.contextInsertRangeAll r c) c

-- Also insert range, unless in builtin module.
contextRename
  :: MonadReader Environment m
  => Name
  -> Name
  -> Range
  -> Context
  -> m Context
contextRename n t r c
  = contextInsertRange n r c
  >>= pure . C.contextRename n t

-- Also insert range, unless in builtin module.
contextRenameModule
  :: MonadReader Environment m
  => Name
  -> Name
  -> Range
  -> Context
  -> m Context
contextRenameModule n t r c
  = contextInsertRangeModule n r c
  >>= pure . C.contextRenameModule n t

-- ## Syntax

syntax
  :: Fixities
  -> Name
  -> Maybe Name
syntax fs (Name ps)
  = fromFixity <$> Map.lookup (N.Name NoRange InScope ps) fs

fromFixity
  :: Fixity'
  -> Name
fromFixity (Fixity' _ ps _)
  = Name (fromGenPart <$> ps)

fromGenPart
  :: GenPart
  -> NamePart
fromGenPart (BindHole _ _)
  = Hole
fromGenPart (NormalHole _ _)
  = Hole
fromGenPart (WildHole _)
  = Hole
fromGenPart (IdPart (Ranged _ s))
  = Id s

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
  -> Fixities
  -> Access
  -> RangeType
  -> Range
  -> Name
  -> m AccessContext
checkName _ _ _ _ NoRange _
  = pure mempty
checkName _ _ _ _ _ (Name [Hole])
  = pure mempty
checkName b fs a t r@(Range _ _) n
  = bool (pure ()) (modifyInsert r (RangeInfo t (QName n))) b
  >> pure (accessContextItem n a (item [r] (syntax fs n)))

checkName'
  :: MonadReader Environment m
  => MonadState State m
  => Bool
  -- ^ Whether to add new names to state.
  -> Fixities
  -> Access
  -> RangeType
  -> N.Name
  -> m AccessContext
checkName' b fs a t n
  = maybe (pure mempty) (uncurry (checkName b fs a t)) (fromNameRange n)

checkNames'
  :: MonadReader Environment m
  => MonadState State m
  => Access
  -> RangeType
  -> [N.Name]
  -> m AccessContext
checkNames' a
  = checkSequence (checkName' True mempty a)

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

checkQNameP'
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => AccessContext
  -> N.QName
  -> m AccessContext
checkQNameP' c n
  = maybe (pure mempty) (uncurry (checkQNameP c)) (fromQNameRange n)

checkQName
  :: MonadReader Environment m
  => MonadState State m
  => Access
  -> RangeType
  -> Range
  -> QName
  -> m AccessContext
checkQName a t r (QName n)
  = checkName True mempty a t r n
checkQName _ _ _ (Qual _ _)
  = pure mempty

touchName
  :: MonadReader Environment m
  => MonadState State m
  => AccessContext
  -> Name
  -> m ()
touchName c n
  = askBuiltin
  >>= \b -> touchNameWith b (accessContextLookupDefining (QName n) c)

touchNameWith
  :: MonadReader Environment m
  => MonadState State m
  => Bool
  -- ^ Whether we are in a builtin module.
  -> Either LookupError (Bool, [Range])
  -> m ()
touchNameWith False (Right (False, rs))
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
  = askBuiltin
  >>= \b -> touchQNameWith b (accessContextLookupDefining n c) r n

touchQNameWith
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => Bool
  -- ^ Whether we are in a builtin module.
  -> Either LookupError (Bool, [Range])
  -> Range
  -> QName
  -> m ()
touchQNameWith False (Left LookupAmbiguous) r n
  = throwError (ErrorAmbiguous r n)
touchQNameWith b rs _ _
  = touchNameWith b rs

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
  = checkName' b mempty Public RangeVariable n
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
  => AccessContext
  -> Pattern
  -> m AccessContext
checkPattern c (IdentP n)
  = checkQNameP' c n
checkPattern _ (QuoteP _)
  = pure mempty
checkPattern c (AppP p (Arg _ (Named _ p')))
  = (<>) <$> checkPattern c p
    <*> checkPattern c p'
checkPattern c (RawAppP _ ps)
  = checkRawAppP c ps
checkPattern _ (OpAppP r _ _ _)
  = throwError (ErrorInternal (ErrorUnexpected UnexpectedOpAppP) r)
checkPattern c (HiddenP _ (Named _ p))
  = checkPattern c p
checkPattern c (InstanceP _ (Named _ p))
  = checkPattern c p
checkPattern c (ParenP _ p)
  = checkPattern c p
checkPattern _ (WildP _)
  = pure mempty
checkPattern _ (AbsurdP _)
  = pure mempty
checkPattern c (AsP _ n p)
  = (<>) <$> checkName' True mempty Public RangeVariable n
    <*> checkPattern c p
checkPattern c (DotP _ e)
  = checkExpr c e >> pure mempty
checkPattern _ (LitP _)
  = pure mempty
checkPattern c (RecP _ as)
  = checkPatterns c (_exprFieldA <$> as)
checkPattern c (EqualP _ es)
  = checkExprPairs c es >> pure mempty
checkPattern _ (EllipsisP _)
  = pure mempty
checkPattern c (WithP _ p)
  = checkPattern c p

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

checkRawAppP
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => AccessContext
  -> [Pattern]
  -> m AccessContext
checkRawAppP c ps
  = pure (accessContextMatch (patternNames ps) c)
  >>= \ns -> touchNames c ns
  >> checkPatterns c (patternDelete ns ps)

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
  = touchNames c (accessContextMatch (exprNames es) c)
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
  => AccessContext
  -> LHS
  -> m AccessContext
checkLHS c (LHS p rs ws _)
  = checkPattern c p
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
checkLamClause c (LamClause l r _ _)
  = checkLHS c l
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
  >>= \c' -> pure (accessContextModule' n' (fromAccess a) c', c')

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
  = checkExpr c e >> checkPattern c p

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
  >> checkPattern (c <> c') p
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
checkDoStmts c ss
  = bool (pure ()) (touchName c bind) (hasBind ss)
  >> bool (pure ()) (touchName c bind_) (hasBind_ ss)
  >> checkFold checkDoStmt c ss

hasBind
  :: [DoStmt]
  -> Bool
hasBind []
  = False
hasBind (_ : [])
  = False
hasBind (DoBind _ _ _ _ : _ : _)
  = True
hasBind (_ : ss)
  = hasBind ss

hasBind_
  :: [DoStmt]
  -> Bool
hasBind_ []
  = False
hasBind_ (_ : [])
  = False
hasBind_ (DoThen _ : _ : _)
  = True
hasBind_ (_ : ss)
  = hasBind_ ss

bind
  :: Name
bind
  = Name
  [ Hole
  , Id ">>="
  , Hole
  ]

bind_
  :: Name
bind_
  = Name
  [ Hole
  , Id ">>"
  , Hole
  ]

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

checkNiceDeclaration fs c (Axiom _ a _ _ _ n e)
  = checkExpr c e >> checkName' True fs (fromAccess a) RangeDefinition n
checkNiceDeclaration _ _ (NiceField r _ _ _ _ _ _)
  = throwError (ErrorInternal (ErrorUnexpected UnexpectedField) r)
checkNiceDeclaration fs c (PrimitiveFunction _ a _ n e)
  = checkExpr c e >> checkName' True fs (fromAccess a) RangeDefinition n
checkNiceDeclaration fs c (NiceMutual _ _ _ _ ds)
  = checkNiceDeclarations fs c ds
checkNiceDeclaration _ c (NiceModule _ a _ (N.QName n) bs ds)
  = checkNiceModule c (fromAccess a) (fromName n) bs ds
checkNiceDeclaration _ _ (NiceModule _ _ _ n@(N.Qual _ _) _ _)
  = throwError (ErrorInternal ErrorName (getRange n))
checkNiceDeclaration _ _ (NicePragma _ _)
  = pure mempty
checkNiceDeclaration fs c (NiceRecSig _ a _ _ _ n bs e)
  = checkNiceSig fs c a RangeRecord n bs e
checkNiceDeclaration fs c (NiceDataSig _ a _ _ _ n bs e)
  = checkNiceSig fs c a RangeData n bs e
checkNiceDeclaration _ _ (NiceFunClause r _ _ _ _ _ _)
  = throwError (ErrorInternal (ErrorUnexpected UnexpectedNiceFunClause) r)
checkNiceDeclaration fs c (FunSig _ a _ _ _ _ _ _ n e)
  = checkExpr c e >> checkName' True fs (fromAccess a) RangeDefinition n
checkNiceDeclaration _ c (FunDef _ _ _ _ _ _ _ cs)
  = checkClauses c cs >> pure mempty
checkNiceDeclaration _ c (NiceGeneralize _ _ _ _ _ e)
  = checkExpr c e >> pure mempty
checkNiceDeclaration _ _ (NiceUnquoteDecl r _ _ _ _ _ _ _)
  = throwError (ErrorUnsupported UnsupportedUnquote r)
checkNiceDeclaration _ _ (NiceUnquoteDef r _ _ _ _ _ _)
  = throwError (ErrorUnsupported UnsupportedUnquote r)

checkNiceDeclaration _ c
  (NiceModuleMacro r _ (N.NoName _ _)
    (SectionApp _ [] (RawApp _ (Ident n : es))) DoOpen i)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromQName n)
  >>= \n' -> liftLookup r n' (accessContextLookupModule n' c)
  >>= \c' -> checkExprs c es
  >> checkImportDirective Open r n' c' i
  >>= pure . fromContext (importDirectiveAccess i)
checkNiceDeclaration _ _
  (NiceModuleMacro r _ _ _ _ _)
  = throwError (ErrorUnsupported UnsupportedMacro r)

checkNiceDeclaration _ c (NiceOpen r n i)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromQName n)
  >>= \n' -> liftLookup r n' (accessContextLookupModule n' c)
  >>= \c' -> checkImportDirective Open r n' c' i
  >>= pure . fromContext (importDirectiveAccess i)

checkNiceDeclaration _ _ (NiceImport r n Nothing DontOpen i)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromQName n)
  >>= \n' -> checkFile (Just r) n'
  >>= \c' -> checkImportDirective Import r n' c' i
  >>= \c'' -> pure (accessContextImport n' c'')
checkNiceDeclaration _ _ (NiceImport r n Nothing DoOpen i)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromQName n)
  >>= \n' -> checkFile (Just r) n'
  >>= \c' -> checkImportDirective Import r n' c' i
  >>= \c'' -> pure (accessContextImport n' c'
    <> fromContext (importDirectiveAccess i) c'')
checkNiceDeclaration _ _ (NiceImport r n (Just a) DontOpen i)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromQName n)
  >>= \n' -> liftMaybe (ErrorInternal ErrorName (getRange a)) (fromAsName a)
  >>= \a' -> checkFile (Just r) n'
  >>= \c' -> checkImportDirective Import r n' c' i
  >>= \c'' -> pure (accessContextModule a' Public c'')
checkNiceDeclaration _ _ (NiceImport r n (Just a) DoOpen i)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromQName n)
  >>= \n' -> liftMaybe (ErrorInternal ErrorName (getRange a)) (fromAsName a)
  >>= \a' -> checkFile (Just r) n'
  >>= \c' -> checkImportDirective Import r n' c' i
  >>= \c'' -> pure (accessContextModule a' Public c''
    <> fromContext (importDirectiveAccess i) c'')

checkNiceDeclaration fs c (NiceDataDef _ _ _ _ _ n bs cs)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromName n)
  >>= \n' -> pure (either (const []) id (accessContextLookup (QName n') c))
  >>= \rs -> checkLamBindings False c bs
  >>= \c' -> checkNiceConstructors fs rs (accessContextDefine n' c <> c') cs
  >>= \c'' -> pure (accessContextModule' n' Public c'' <> c'')

checkNiceDeclaration fs c (NiceRecDef _ _ _ _ _ n _ _ m bs ds)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromName n)
  >>= \n' -> pure (either (const []) id (accessContextLookup (QName n') c))
  >>= \rs -> checkLamBindings False c bs
  >>= \c' -> checkNiceConstructorRecordMay fs rs (m >>= fromNameRange . fst)
  >>= \c'' -> checkDeclarationsRecord n' rs (c <> c') ds
  >>= \c''' -> pure (accessContextModule' n' Public (c'' <> c''') <> c'')

checkNiceDeclaration fs c (NicePatternSyn _ a n ns p)
  = checkNames' Public RangeVariable (unArg <$> ns)
  >>= \c' -> checkPattern (c <> c') p
  >> checkName' True fs (fromAccess a) RangePatternSynonym n

checkNiceDeclarationRecord
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Name
  -> [Range]
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
checkNiceDeclarationRecord _ _ fs c d@(NiceRecDef _ _ _ _ _ _ _ _ _ _ _)
  = checkNiceDeclaration fs c d
checkNiceDeclarationRecord _ _ fs c d@(NicePatternSyn _ _ _ _ _)
  = checkNiceDeclaration fs c d
checkNiceDeclarationRecord _ _ fs c d@(NiceGeneralize _ _ _ _ _ _)
  = checkNiceDeclaration fs c d
checkNiceDeclarationRecord _ _ fs c d@(NiceUnquoteDecl _ _ _ _ _ _ _ _)
  = checkNiceDeclaration fs c d
checkNiceDeclarationRecord _ _ fs c d@(NiceUnquoteDef _ _ _ _ _ _ _)
  = checkNiceDeclaration fs c d

checkNiceDeclarationRecord n rs fs c (NiceField _ a _ _ _ n' (Arg _ e))
  = checkExpr (accessContextDefine n c) e
  >> maybe
    (pure mempty)
    (\n'' -> pure (accessContextItem n'' (fromAccess a)
      (item rs (syntax fs n''))))
    (fromName n')

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
  -> [Range]
  -- ^ Ranges associated with parent record.
  -> Fixities
  -> AccessContext
  -> [NiceDeclaration]
  -> m AccessContext
checkNiceDeclarationsRecord n rs fs
  = checkFoldUnion (checkNiceDeclarationRecord n rs fs)

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
checkNiceDeclarationsTop _ c (NiceModule _ a _ _ bs ds : _)
  = checkNiceModule c (fromAccess a) Nothing bs ds
checkNiceDeclarationsTop fs c (d : ds)
  = checkNiceDeclaration fs c d >>= \c' -> checkNiceDeclarations fs (c <> c') ds

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
  >> checkName' True fs (fromAccess a) t n

checkNiceConstructor
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Fixities
  -> [Range]
  -- ^ Ranges associated with parent type.
  -> AccessContext
  -> NiceDeclaration
  -> m AccessContext
checkNiceConstructor fs rs c (Axiom _ a _ _ _ n e)
  = checkExpr c e
  >> maybe
    (pure mempty)
    (\n'' -> pure (accessContextItem n'' (fromAccess a)
      (itemConstructor rs (syntax fs n''))))
    (fromName n)
checkNiceConstructor _ _ _ d
  = throwError (ErrorInternal ErrorConstructor (getRange d))

checkNiceConstructors
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Fixities
  -> [Range]
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
  -> [Range]
  -- ^ Ranges associated with record type.
  -> Range
  -> Name
  -> m AccessContext
checkNiceConstructorRecord fs rs r n
  = modifyInsert r (RangeInfo RangeRecordConstructor (QName n))
  >> pure (accessContextItem n Public (itemConstructor (r : rs) (syntax fs n)))

checkNiceConstructorRecordMay
  :: MonadReader Environment m
  => MonadState State m
  => Fixities
  -> [Range]
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
  -> Maybe Name
  -- ^ If `Nothing`, the module is anonymous.
  -> [TypedBinding]
  -> [Declaration]
  -> m AccessContext
checkNiceModule c a n bs ds
  = checkTypedBindings c bs
  >>= \c' -> checkDeclarations (c <> c') ds
  >>= pure . maybe (fromContext a) (flip accessContextModule a) n . toContext

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
  >>= contextInsertRangeAll r
checkImportDirective t r n c (ImportDirective _ (Using ns) _ rs _)
  = modifyInsert r (RangeInfo (directiveStatement t) n)
  >> checkImportedNames t c ns
  >>= \c' -> checkRenamings t c rs
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
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromName n)
  >>= \n' -> liftMaybe (ErrorInternal ErrorName (getRange t)) (fromNameRange t)
  >>= \(r, t') -> modifyInsert r (RangeInfo (directiveItem dt) (QName t'))
  >> pure (maybe mempty (contextItem t') (contextLookupItem (QName n') c)
    <> maybe mempty (contextModule t') (contextLookupModule (QName n') c))
  >>= contextInsertRangeAll r
checkImportedNamePair dt c (_, ImportedModule n, ImportedModule t)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromName n)
  >>= \n' -> liftMaybe (ErrorInternal ErrorName (getRange t)) (fromNameRange t)
  >>= \(r, t') -> modifyInsert r (RangeInfo (directiveItem dt) (QName t'))
  >> pure (maybe mempty (contextModule t') (contextLookupModule (QName n') c))
  >>= contextInsertRangeAll r
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
  >> contextRename n' t' r c
  >>= contextRenameModule n' t' r
modifyRenaming dt c (Renaming (ImportedModule n) (ImportedModule t) _ _)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromName n)
  >>= \n' -> liftMaybe (ErrorInternal ErrorName (getRange t)) (fromNameRange t)
  >>= \(r, t') -> modifyInsert r (RangeInfo (directiveItem dt) (QName n'))
  >> contextRenameModule n' t' r c
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
  = toContext <$> checkDeclarationsTop mempty ds

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

