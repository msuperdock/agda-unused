{- |
Module: Agda.Unused

Check an Agda project for unused code.
-}
module Agda.Unused.Check
  ( checkUnused
  , checkUnusedLocal
  ) where

import Agda.Unused
  (Unused(..), UnusedItems(..))
import Agda.Unused.Monad.Error
  (Error(..), InternalError(..), UnexpectedError(..), UnsupportedError(..),
    liftLookup)
import Agda.Unused.Monad.Reader
  (Environment(..), askRoot, askSkip, localSkip)
import Agda.Unused.Monad.State
  (ModuleState(..), State(..), modifyDelete, stateBlock, stateCheck, stateEmpty,
    stateLookup)
import qualified Agda.Unused.Monad.State
  as S
import Agda.Unused.Types.Access
  (Access(..), fromAccess)
import Agda.Unused.Types.Context
  (AccessContext, Context, LookupError(..), accessContextDefine,
    accessContextImport, accessContextItem, accessContextLookup,
    accessContextLookupAccess, accessContextLookupDefining,
    accessContextLookupModule, accessContextLookupSpecial, accessContextMatch,
    accessContextModule, accessContextModule', accessContextUnion,
    contextDelete, contextDeleteModule, contextItem, contextLookup,
    contextLookupItem, contextLookupModule, contextModule, contextRanges,
    fromContext, item, itemConstructor, itemPattern, toContext)
import qualified Agda.Unused.Types.Context
  as C
import Agda.Unused.Types.Name
  (Name(..), QName(..), isBuiltin, fromAsName, fromName, fromNameRange,
    fromQName, fromQNameRange, nameIds, pathQName, qNamePath)
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
  (MonadReader, ReaderT, runReaderT)
import Control.Monad.State
  (MonadState, StateT, gets, modify, runStateT)
import Data.Bool
  (bool)
import qualified Data.Map.Strict
  as Map
import Data.Maybe
  (catMaybes)
import qualified Data.Text
  as T
import System.Directory
  (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath
  ((</>))

import Paths_agda_unused
  (getDataFileName)

-- ## Context

-- Do nothing if in builtin module.
contextInsertRange
  :: MonadReader Environment m
  => Name
  -> Range
  -> Context
  -> m Context
contextInsertRange n r c
  = askSkip >>= pure . bool (C.contextInsertRange n r c) c

-- Do nothing if in builtin module.
contextInsertRangeModule
  :: MonadReader Environment m
  => Name
  -> Range
  -> Context
  -> m Context
contextInsertRangeModule n r c
  = askSkip >>= pure . bool (C.contextInsertRangeModule n r c) c

-- Do nothing if in builtin module.
contextInsertRangeAll
  :: MonadReader Environment m
  => Range
  -> Context
  -> m Context
contextInsertRangeAll r c
  = askSkip >>= pure . bool (C.contextInsertRangeAll r c) c

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

-- ## State

modifyInsert
  :: MonadReader Environment m
  => MonadState State m
  => Bool
  -- ^ Whether to check public names.
  -> Access
  -> Range
  -> RangeInfo
  -> m ()
modifyInsert False Public _ _
  = pure ()
modifyInsert _ _ r i
  = S.modifyInsert r i

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
fromFixity (Fixity' _ [] _)
  = Nothing
fromFixity (Fixity' _ ps@(_ : _) _)
  = Just (Name (fromGenPart <$> ps))

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
  -- ^ Whether to check public names.
  -> Bool
  -- ^ Whether to treat names as pattern synonyms.
  -> Fixities
  -> Access
  -> RangeType
  -> Range
  -> Name
  -> m AccessContext
checkName _ _ _ _ _ NoRange _
  = pure mempty
checkName _ _ _ _ _ _ (Name [Hole])
  = pure mempty
checkName b p fs a t r@(Range _ _) n
  = modifyInsert b a r (RangeInfo t (QName n))
  >> pure (accessContextItem n a (bool item itemPattern p [r] (syntax fs n)))

checkName'
  :: MonadReader Environment m
  => MonadState State m
  => Bool
  -- ^ Whether to check public names.
  -> Bool
  -- ^ Whether to treat names as pattern synonyms.
  -> Fixities
  -> Access
  -> RangeType
  -> N.Name
  -> m AccessContext
checkName' b p fs a t n
  = maybe (pure mempty) (uncurry (checkName b p fs a t)) (fromNameRange n)

checkNames'
  :: MonadReader Environment m
  => MonadState State m
  => Bool
  -- ^ Whether to check public names.
  -> Bool
  -- ^ Whether to treat names as pattern synonyms.
  -> Access
  -> RangeType
  -> [N.Name]
  -> m AccessContext
checkNames' b p a
  = checkSequence (checkName' b p mempty a)

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
  = checkName True False mempty a t r n
checkQName _ _ _ (Qual _ _)
  = pure mempty

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
  => Either LookupError (Bool, [Range])
  -> Bool
  -- ^ Whether we are in a builtin module.
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
  = askSkip >>= touchQNameWith r n (accessContextLookupDefining n c)

touchQNameWith
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => Range
  -> QName
  -> Either LookupError (Bool, [Range])
  -> Bool
  -- ^ Whether we are in a builtin module.
  -> m ()
touchQNameWith r n (Left LookupAmbiguous) False
  = throwError (ErrorAmbiguous r n)
touchQNameWith _ _ rs b
  = touchNameWith rs b

touchQNameContext
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => Context
  -> QName
  -> QName
  -> m ()
touchQNameContext c m n
  = maybe (throwError (ErrorRoot m n)) modifyDelete (contextLookup n c)

touchQNamesContext
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => Context
  -> QName
  -> [QName]
  -> m ()
touchQNamesContext m p
  = void . traverse (touchQNameContext m p)

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
  -- ^ Whether to check bound names.
  -> AccessContext
  -> Binder
  -> m AccessContext
checkBinder b c (Binder p (BName n _ _))
  = bool localSkip id b (checkName' True False mempty Public RangeVariable n)
  >>= \c' -> checkPatternMay c p
  >>= \c'' -> pure (c' <> c'')

checkBinders
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Bool
  -- ^ Whether to check bound names.
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
  = checkExpr c e >> checkBinders b c (namedThing . unArg <$> bs)
checkTypedBinding _ c (TLet _ ds)
  = checkDeclarations True c ds

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

checkPattern c (AppP p (Arg _ (Named _ p')))
  = (<>)
  <$> checkPattern c p
  <*> checkPattern c p'

checkPattern c (AsP _ n p)
  = (<>)
  <$> checkName' True False mempty Public RangeVariable n
  <*> checkPattern c p

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
  = checkTypedBindings True c bs >>= \c' -> checkExpr (c <> c') e
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

checkExpr c (Let _ ds e)
  = checkDeclarations True c ds
  >>= \c' -> maybe (pure ()) (checkExpr (c <> c')) e

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
  = checkDeclarations True c ds
  >>= \c' -> pure (mempty, c')
checkWhereClause c (SomeWhere n a ds)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromName n)
  >>= \n' -> checkDeclarations True c ds
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
  = checkDeclarations True c ds

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
  => Bool
  -- ^ Whether to check public names.
  -> AccessContext
  -> [Declaration]
  -> m AccessContext
checkDeclarations b
  = checkDeclarationsWith (checkNiceDeclarations b)

checkDeclarationsRecord
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Bool
  -- ^ Whether to check public names.
  -> Name
  -> [Range]
  -- ^ Ranges associated with parent record.
  -> AccessContext
  -> [Declaration]
  -> m AccessContext
checkDeclarationsRecord b n rs
  = checkDeclarationsWith (checkNiceDeclarationsRecord b n rs)

checkDeclarationsTop
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Bool
  -- ^ Whether to check public names.
  -> AccessContext
  -> [Declaration]
  -> m AccessContext
checkDeclarationsTop b
  = checkDeclarationsWith (checkNiceDeclarationsTop b)

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
  => Bool
  -- ^ Whether to check public names.
  -> Fixities
  -> AccessContext
  -> NiceDeclaration
  -> m AccessContext

checkNiceDeclaration b fs c (Axiom _ a _ _ _ n e)
  = checkExpr c e >> checkName' b False fs (fromAccess a) RangeDefinition n
checkNiceDeclaration _ _ _ (NiceField r _ _ _ _ _ _)
  = throwError (ErrorInternal (ErrorUnexpected UnexpectedField) r)
checkNiceDeclaration b fs c (PrimitiveFunction _ a _ n e)
  = checkExpr c e >> checkName' b False fs (fromAccess a) RangeDefinition n
checkNiceDeclaration b fs c (NiceMutual _ _ _ _ ds)
  = checkNiceDeclarations b fs c ds
checkNiceDeclaration b _ c (NiceModule _ a _ (N.QName n) bs ds)
  = checkNiceModule b c (fromAccess a) (fromName n) bs ds
checkNiceDeclaration _ _ _ (NiceModule _ _ _ n@(N.Qual _ _) _ _)
  = throwError (ErrorInternal ErrorName (getRange n))
checkNiceDeclaration _ _ _ (NicePragma _ _)
  = pure mempty
checkNiceDeclaration b fs c (NiceRecSig _ a _ _ _ n bs e)
  = checkNiceSig b fs c a RangeRecord n bs e
checkNiceDeclaration b fs c (NiceDataSig _ a _ _ _ n bs e)
  = checkNiceSig b fs c a RangeData n bs e
checkNiceDeclaration _ _ _ (NiceFunClause r _ _ _ _ _ _)
  = throwError (ErrorInternal (ErrorUnexpected UnexpectedNiceFunClause) r)
checkNiceDeclaration b fs c (FunSig _ a _ _ _ _ _ _ n e)
  = checkExpr c e >> checkName' b False fs (fromAccess a) RangeDefinition n
checkNiceDeclaration _ _ c (FunDef _ _ _ _ _ _ _ cs)
  = checkClauses c cs >> pure mempty
checkNiceDeclaration _ _ c (NiceGeneralize _ _ _ _ _ e)
  = checkExpr c e >> pure mempty
checkNiceDeclaration _ _ _ (NiceUnquoteDecl r _ _ _ _ _ _ _)
  = throwError (ErrorUnsupported UnsupportedUnquote r)
checkNiceDeclaration _ _ _ (NiceUnquoteDef r _ _ _ _ _ _)
  = throwError (ErrorUnsupported UnsupportedUnquote r)

checkNiceDeclaration b _ c
  (NiceModuleMacro r _ (N.NoName _ _)
    (SectionApp _ [] (RawApp _ (Ident n : es))) DoOpen i)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromQName n)
  >>= \n' -> liftLookup r n' (accessContextLookupModule n' c)
  >>= \c' -> checkExprs c es
  >> checkImportDirective Open b r n' c' i
  >>= pure . fromContext (importDirectiveAccess i)
checkNiceDeclaration b _ c
  (NiceModuleMacro r _ a
    (SectionApp _ [] (RawApp _ (Ident n : es))) DontOpen i)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromQName n)
  >>= \n' -> liftMaybe (ErrorInternal ErrorName (getRange a)) (fromName a)
  >>= \a' -> liftLookup r n' (accessContextLookupModule n' c)
  >>= \c' -> checkExprs c es
  >> checkImportDirective Macro b r (QName a') c' i
  >>= \c'' -> pure (accessContextModule a' Public c'')
checkNiceDeclaration b _ c
  (NiceModuleMacro r _ a
    (SectionApp _ [] (RawApp _ (Ident n : es))) DoOpen i)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromQName n)
  >>= \n' -> liftMaybe (ErrorInternal ErrorName (getRange a)) (fromName a)
  >>= \a' -> liftLookup r n' (accessContextLookupModule n' c)
  >>= \c' -> checkExprs c es
  >> checkImportDirective Macro b r (QName a') c' i
  >>= \c'' -> pure (accessContextModule a' Public c''
    <> fromContext (importDirectiveAccess i) c'')
checkNiceDeclaration _ _ _
  (NiceModuleMacro _ _ _
    (SectionApp r _ _) _ _)
  = throwError (ErrorInternal ErrorMacro r)
checkNiceDeclaration _ _ _
  (NiceModuleMacro r _ _
    (RecordModuleInstance _ _) _ _)
  = throwError (ErrorUnsupported UnsupportedMacro r)

checkNiceDeclaration b _ c (NiceOpen r n i)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromQName n)
  >>= \n' -> liftLookup r n' (accessContextLookupModule n' c)
  >>= \c' -> checkImportDirective Open b r n' c' i
  >>= \c'' -> pure (fromContext (importDirectiveAccess i) c'')

checkNiceDeclaration b _ _ (NiceImport r n Nothing DontOpen i)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromQName n)
  >>= \n' -> checkFile b (Just r) n'
  >>= \c' -> checkImportDirective Import b r n' c' i
  >>= \c'' -> pure (accessContextImport n' c'')
checkNiceDeclaration b _ _ (NiceImport r n Nothing DoOpen i)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromQName n)
  >>= \n' -> checkFile b (Just r) n'
  >>= \c' -> checkImportDirective Import b r n' c' i
  >>= \c'' -> pure (accessContextImport n' c'
    <> fromContext (importDirectiveAccess i) c'')
checkNiceDeclaration b _ _ (NiceImport r n (Just a) DontOpen i)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromQName n)
  >>= \n' -> liftMaybe (ErrorInternal ErrorName (getRange a)) (fromAsName a)
  >>= \a' -> checkFile b (Just r) n'
  >>= \c' -> checkImportDirective Import b r n' c' i
  >>= \c'' -> pure (accessContextModule a' Public c'')
checkNiceDeclaration b _ _ (NiceImport r n (Just a) DoOpen i)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromQName n)
  >>= \n' -> liftMaybe (ErrorInternal ErrorName (getRange a)) (fromAsName a)
  >>= \a' -> checkFile b (Just r) n'
  >>= \c' -> checkImportDirective Import b r n' c' i
  >>= \c'' -> pure (accessContextModule a' Public c''
    <> fromContext (importDirectiveAccess i) c'')

checkNiceDeclaration _ fs c (NiceDataDef _ _ _ _ _ n bs cs)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromName n)
  >>= \n' -> pure (either (const []) id (accessContextLookup (QName n') c))
  >>= \rs -> checkLamBindings False c bs
  >>= \c' -> checkNiceConstructors fs rs (accessContextDefine n' c <> c') cs
  >>= \c'' -> pure (accessContextModule' n' Public c'' <> c'')

checkNiceDeclaration b fs c (NiceRecDef _ _ _ _ _ n _ _ m bs ds)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromName n)
  >>= \n' -> liftMaybe (ErrorInternal ErrorName (getRange n))
    (accessContextLookupAccess n' c)
  >>= \(a, rs) -> checkLamBindings False c bs
  >>= \c' -> checkNiceConstructorRecordMay b fs a rs (m >>= fromNameRange . fst)
  >>= \c'' -> checkDeclarationsRecord b n' rs (c <> c') ds
  >>= \c''' -> pure (accessContextModule' n' Public (c'' <> c''') <> c'')

checkNiceDeclaration b fs c (NicePatternSyn _ a n ns p)
  = localSkip (checkNames' True False Public RangeVariable (unArg <$> ns))
  >>= \c' -> checkPattern (c <> c') p
  >> checkName' b True fs (fromAccess a) RangePatternSynonym n

checkNiceDeclarationRecord
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Bool
  -- ^ Whether to check public names.
  -> Name
  -> [Range]
  -- ^ Ranges associated with parent record.
  -> Fixities
  -> AccessContext
  -> NiceDeclaration
  -> m AccessContext
checkNiceDeclarationRecord b _ rs fs c d@(Axiom _ _ _ _ _ _ _)
  = modifyDelete rs >> checkNiceDeclaration b fs c d
checkNiceDeclarationRecord b _ rs fs c d@(PrimitiveFunction _ _ _ _ _)
  = modifyDelete rs >> checkNiceDeclaration b fs c d
checkNiceDeclarationRecord b n rs fs c (NiceMutual _ _ _ _ ds)
  = checkNiceDeclarationsRecord b n rs fs c ds
checkNiceDeclarationRecord b _ rs fs c d@(NiceModule _ _ _ _ _ _)
  = modifyDelete rs >> checkNiceDeclaration b fs c d
checkNiceDeclarationRecord b _ _ fs c d@(NiceModuleMacro _ _ _ _ _ _)
  = checkNiceDeclaration b fs c d
checkNiceDeclarationRecord b _ _ fs c d@(NiceOpen _ _ _)
  = checkNiceDeclaration b fs c d
checkNiceDeclarationRecord b _ _ fs c d@(NiceImport _ _ _ _ _)
  = checkNiceDeclaration b fs c d
checkNiceDeclarationRecord b _ _ fs c d@(NicePragma _ _)
  = checkNiceDeclaration b fs c d
checkNiceDeclarationRecord b _ rs fs c d@(NiceRecSig _ _ _ _ _ _ _ _)
  = modifyDelete rs >> checkNiceDeclaration b fs c d
checkNiceDeclarationRecord b _ rs fs c d@(NiceDataSig _ _ _ _ _ _ _ _)
  = modifyDelete rs >> checkNiceDeclaration b fs c d
checkNiceDeclarationRecord b _ _ fs c d@(NiceFunClause _ _ _ _ _ _ _)
  = checkNiceDeclaration b fs c d
checkNiceDeclarationRecord b _ rs fs c d@(FunSig _ _ _ _ _ _ _ _ _ _)
  = modifyDelete rs >> checkNiceDeclaration b fs c d
checkNiceDeclarationRecord b _ _ fs c d@(FunDef _ _ _ _ _ _ _ _)
  = checkNiceDeclaration b fs c d
checkNiceDeclarationRecord b _ _ fs c d@(NiceDataDef _ _ _ _ _ _ _ _)
  = checkNiceDeclaration b fs c d
checkNiceDeclarationRecord b _ _ fs c d@(NiceRecDef _ _ _ _ _ _ _ _ _ _ _)
  = checkNiceDeclaration b fs c d
checkNiceDeclarationRecord b _ _ fs c d@(NicePatternSyn _ _ _ _ _)
  = checkNiceDeclaration b fs c d
checkNiceDeclarationRecord b _ _ fs c d@(NiceGeneralize _ _ _ _ _ _)
  = checkNiceDeclaration b fs c d
checkNiceDeclarationRecord b _ _ fs c d@(NiceUnquoteDecl _ _ _ _ _ _ _ _)
  = checkNiceDeclaration b fs c d
checkNiceDeclarationRecord b _ _ fs c d@(NiceUnquoteDef _ _ _ _ _ _ _)
  = checkNiceDeclaration b fs c d

checkNiceDeclarationRecord _ n rs fs c (NiceField _ a _ _ _ n' (Arg _ e))
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
  => Bool
  -- ^ Whether to check public names.
  -> Fixities
  -> AccessContext
  -> [NiceDeclaration]
  -> m AccessContext
checkNiceDeclarations b fs
  = checkFoldUnion (checkNiceDeclaration b fs)

checkNiceDeclarationsRecord
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Bool
  -- ^ Whether to check public names.
  -> Name
  -> [Range]
  -- ^ Ranges associated with parent record.
  -> Fixities
  -> AccessContext
  -> [NiceDeclaration]
  -> m AccessContext
checkNiceDeclarationsRecord b n rs fs
  = checkFoldUnion (checkNiceDeclarationRecord b n rs fs)

checkNiceDeclarationsTop
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Bool
  -- ^ Whether to check public names.
  -> Fixities
  -> AccessContext
  -> [NiceDeclaration]
  -> m AccessContext
checkNiceDeclarationsTop _ _ _ []
  = pure mempty
checkNiceDeclarationsTop b _ c (NiceModule _ a _ _ bs ds : _)
  = checkNiceModule b c (fromAccess a) Nothing bs ds
checkNiceDeclarationsTop b fs c (d : ds)
  = checkNiceDeclaration b fs c d
  >>= \c' -> checkNiceDeclarations b fs (c <> c') ds

checkNiceSig
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Bool
  -- ^ Whether to check public names.
  -> Fixities
  -> AccessContext
  -> Common.Access
  -> RangeType
  -> N.Name
  -> [LamBinding]
  -> Expr
  -> m AccessContext
checkNiceSig b fs c a t n bs e
  = checkLamBindings False c bs
  >>= \c' -> checkExpr (c <> c') e
  >> checkName' b False fs (fromAccess a) t n

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
  => Bool
  -- ^ Whether to check public names.
  -> Fixities
  -> Access
  -- ^ Access modifier of record type.
  -> [Range]
  -- ^ Ranges associated with record type.
  -> Range
  -> Name
  -> m AccessContext
checkNiceConstructorRecord b fs a rs r n
  = modifyInsert b a r (RangeInfo RangeRecordConstructor (QName n))
  >> pure (accessContextItem n Public (itemConstructor (r : rs) (syntax fs n)))

checkNiceConstructorRecordMay
  :: MonadReader Environment m
  => MonadState State m
  => Bool
  -- ^ Whether to check public names.
  -> Fixities
  -> Access
  -- ^ Access modifier of record type.
  -> [Range]
  -- ^ Ranges associated with record type.
  -> Maybe (Range, Name)
  -> m AccessContext
checkNiceConstructorRecordMay _ _ _ _ Nothing
  = pure mempty
checkNiceConstructorRecordMay b fs a rs (Just (r, n))
  = checkNiceConstructorRecord b fs a rs r n

checkNiceModule
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Bool
  -- ^ Whether to check public names.
  -> AccessContext
  -> Access
  -> Maybe Name
  -- ^ If `Nothing`, the module is anonymous.
  -> [TypedBinding]
  -> [Declaration]
  -> m AccessContext
checkNiceModule b c a n bs ds
  = checkTypedBindings True c bs
  >>= \c' -> checkDeclarations b (c <> c') ds
  >>= pure . maybe (fromContext a) (flip accessContextModule a) n . toContext

-- ## Imports

data DirectiveType where

  Import
    :: DirectiveType

  Macro
    :: DirectiveType

  Open
    :: DirectiveType

  deriving Show

directiveStatement
  :: DirectiveType
  -> RangeType
directiveStatement Import
  = RangeImport
directiveStatement Macro
  = RangeMacro
directiveStatement Open
  = RangeOpen

directiveItem
  :: DirectiveType
  -> RangeType
directiveItem Import
  = RangeImportItem
directiveItem Macro
  = RangeMacroItem
directiveItem Open
  = RangeOpenItem

checkImportDirective
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => DirectiveType
  -> Bool
  -- ^ Whether to check public names.
  -> Range
  -> QName
  -> Context
  -> ImportDirective
  -> m Context
checkImportDirective dt b r n c i@(ImportDirective _ UseEverything hs rs _)
  = modifyInsert b a r (RangeInfo (directiveStatement dt) n)
  >> modifyHidings c hs
  >>= flip (modifyRenamings dt b a) rs
  >>= contextInsertRangeAll r
  where a = importDirectiveAccess i
checkImportDirective dt b r n c i@(ImportDirective _ (Using ns) _ rs _)
  = modifyInsert b a r (RangeInfo (directiveStatement dt) n)
  >> checkImportedNames dt b a c ns
  >>= \c' -> checkRenamings dt b a c rs
  >>= \c'' -> contextInsertRangeAll r (c' <> c'')
  where a = importDirectiveAccess i

checkRenaming
  :: MonadReader Environment m
  => MonadState State m
  => MonadError Error m
  => DirectiveType
  -> Bool
  -- ^ Whether to check public names.
  -> Access
  -> Context
  -> Renaming
  -> m Context
checkRenaming dt b a c r@(Renaming n t _ _)
  = checkImportedNamePair dt b a c (getRange r, n, t)

checkRenamings
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => DirectiveType
  -> Bool
  -- ^ Whether to check public names.
  -> Access
  -> Context
  -> [Renaming]
  -> m Context
checkRenamings dt b a
  = checkSequence (checkRenaming dt b a)

checkImportedName
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => DirectiveType
  -> Bool
  -- ^ Whether to check public names.
  -> Access
  -> Context
  -> ImportedName
  -> m Context
checkImportedName dt b a c n
  = checkImportedNamePair dt b a c (getRange n, n, n)

checkImportedNamePair
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => DirectiveType
  -> Bool
  -- ^ Whether to check public names.
  -> Access
  -> Context
  -> (Range, ImportedName, ImportedName)
  -> m Context
checkImportedNamePair dt b a c (_, ImportedName n, ImportedName t)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromName n)
  >>= \n' -> liftMaybe (ErrorInternal ErrorName (getRange t)) (fromNameRange t)
  >>= \(r, t') -> modifyInsert b a r (RangeInfo (directiveItem dt) (QName t'))
  >> pure (maybe mempty (contextItem t') (contextLookupItem (QName n') c)
    <> maybe mempty (contextModule t') (contextLookupModule (QName n') c))
  >>= contextInsertRangeAll r
checkImportedNamePair dt b a c (_, ImportedModule n, ImportedModule t)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromName n)
  >>= \n' -> liftMaybe (ErrorInternal ErrorName (getRange t)) (fromNameRange t)
  >>= \(r, t') -> modifyInsert b a r (RangeInfo (directiveItem dt) (QName t'))
  >> pure (maybe mempty (contextModule t') (contextLookupModule (QName n') c))
  >>= contextInsertRangeAll r
checkImportedNamePair _ _ _ _ (r, _, _)
  = throwError (ErrorInternal ErrorRenaming r)

checkImportedNames
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => DirectiveType
  -> Bool
  -- ^ Whether to check public names.
  -> Access
  -> Context
  -> [ImportedName]
  -> m Context
checkImportedNames dt b a
  = checkSequence (checkImportedName dt b a)

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
  -> Bool
  -- ^ Whether to check public names.
  -> Access
  -> Context
  -> Renaming
  -> m Context
modifyRenaming dt b a c (Renaming (ImportedName n) (ImportedName t) _ _)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromName n)
  >>= \n' -> liftMaybe (ErrorInternal ErrorName (getRange t)) (fromNameRange t)
  >>= \(r, t') -> modifyInsert b a r (RangeInfo (directiveItem dt) (QName t'))
  >> contextRename n' t' r c
  >>= contextRenameModule n' t' r
modifyRenaming dt b a c (Renaming (ImportedModule n) (ImportedModule t) _ _)
  = liftMaybe (ErrorInternal ErrorName (getRange n)) (fromName n)
  >>= \n' -> liftMaybe (ErrorInternal ErrorName (getRange t)) (fromNameRange t)
  >>= \(r, t') -> modifyInsert b a r (RangeInfo (directiveItem dt) (QName t'))
  >> contextRenameModule n' t' r c
modifyRenaming _ _ _ _ r
  = throwError (ErrorInternal ErrorRenaming (getRange r))

modifyRenamings
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => DirectiveType
  -> Bool
  -- ^ Whether to check public names.
  -> Access
  -> Context
  -> [Renaming]
  -> m Context
modifyRenamings dt b a
  = foldM (modifyRenaming dt b a)

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
  => Bool
  -- ^ Whether to check public names.
  -> Module
  -> m Context
checkModule b (_, ds)
  = toContext <$> checkDeclarationsTop b mempty ds

-- ## Files

checkFile
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Bool
  -- ^ Whether to check public names.
  -> Maybe Range
  -> QName
  -> m Context
checkFile b r n
  = gets (stateLookup n) >>= checkFileWith b r n

checkFileWith
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Bool
  -- ^ Whether to check public names.
  -> Maybe Range
  -> QName
  -> Maybe ModuleState
  -> m Context
checkFileWith b r n Nothing | isBuiltin n
  = liftIO (getDataFileName ("data" </> qNamePath n))
  >>= localSkip . checkFilePath b r n
checkFileWith b r n Nothing
  = askRoot >>= \p -> checkFilePath b r n (p </> qNamePath n)

checkFileWith _ r n (Just Blocked)
  = throwError (ErrorCyclic r n)
checkFileWith _ _ _ (Just (Checked c))
  = pure c

checkFilePath
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Bool
  -- ^ Whether to check public names.
  -> Maybe Range
  -> QName
  -> FilePath
  -> m Context
checkFilePath b r n p = do
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
    <- checkModule b module'
  _
    <- modify (stateCheck n context)
  pure context

-- ## Paths

-- Look for unvisited modules at the given path.
checkPath
  :: MonadIO m
  => [QName]
  -- ^ The visited modules.
  -> FilePath
  -- ^ The project root path.
  -> FilePath
  -- ^ The path at which to look.
  -> m [FilePath]
checkPath ms p p'
  = liftIO (doesDirectoryExist p')
  >>= bool (pure (checkPathFile ms p p')) (checkPathDirectory ms p p')

checkPathFile
  :: [QName]
  -> FilePath
  -> FilePath
  -> [FilePath]
checkPathFile ms p p'
  = maybe [] (bool [p'] [] . flip elem ms) (pathQName p p')

checkPathDirectory
  :: MonadIO m
  => [QName]
  -> FilePath
  -> FilePath
  -> m [FilePath]
checkPathDirectory ms p p'
  = fmap (p' </>) <$> liftIO (listDirectory p')
  >>= traverse (checkPath ms p)
  >>= pure . concat

-- ## Roots

checkRoot
  :: MonadError Error m
  => MonadReader Environment m
  => MonadState State m
  => MonadIO m
  => Root
  -> m ()
checkRoot (Root p ns)
  = checkFile True Nothing p
  >>= \c -> touchQNamesContext c p ns

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

-- | Check an Agda file and its dependencies for unused code.
checkUnused
  :: FilePath
  -- ^ The project root path.
  -> [Root]
  -- ^ A list of public entry points for the project.
  -> IO (Either Error Unused)
checkUnused p rs
  = runExceptT
  $ checkUnusedItems p (checkRoots rs)
  >>= \(State is ms) -> checkPath (Map.keys ms) p p
  >>= \fs -> pure (Unused (UnusedItems is) fs)

-- | Check an Agda file for unused code.
checkUnusedLocal
  :: FilePath
  -- ^ The project root path.
  -> QName
  -- ^ The module to check.
  -> IO (Either Error UnusedItems)
checkUnusedLocal p
  = runExceptT
  . fmap UnusedItems
  . fmap stateItems
  . checkUnusedItems p
  . checkFile False Nothing

checkUnusedItems
  :: MonadError Error m
  => MonadIO m
  => FilePath
  -> ReaderT Environment (StateT State m) a
  -> m State
checkUnusedItems p
  = fmap snd
  . flip runStateT stateEmpty
  . flip runReaderT (Environment False p)

