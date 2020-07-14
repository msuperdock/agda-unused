module Agda.Unused.Context
  ( AccessContext
  , Context
  , (\\)
  , accessContextClear
  , accessContextCons
  , accessContextCons'
  , accessContextDefiningMay
  , accessContextExport
  , accessContextImport
  , accessContextLookup
  , accessContextLookupModule
  , accessContextLookupName
  , accessContextOperators
  , accessContextOperatorsP
  , accessContextPrivate
  , accessContextSingleton
  , accessContextUnion
  , contextCons
  , contextDelete
  , contextDeleteModule
  , contextInsert
  , contextInsertAll
  , contextInsertModule
  , contextLookup
  , contextLookupItem
  , contextLookupModule
  , contextRanges
  , contextRename
  , contextRenameModule
  , contextSingleton
  , fromContext
  ) where

import Agda.Unused.Access
  (Access (..), access)
import Agda.Unused.Error
  (LookupError (..))
import Agda.Unused.Item
  (AccessItem, Item, accessItemDefining, accessItemExport,
    accessItemIsConstructor, accessItemPrivate, accessItemRanges,
    accessItemUnion, fromItem, itemInsert, itemRanges, toItem)
import Agda.Unused.Name
  (Name, QName (..), isOperator, stripPrefix)
import Agda.Unused.Range
  (Range)
import Agda.Unused.Utils
  (mapUpdateKey)

import Data.Map.Strict
  (Map)
import qualified Data.Map.Strict
  as Map
import Data.Maybe
  (catMaybes)

data Context
  = Context
  { contextItems
    :: !(Map Name Item)
  , contextModules
    :: !(Map Name Context)
  } deriving Show

data AccessContext
  = AccessContext
  { accessContextItems
    :: !(Map Name AccessItem)
  , accessContextModules
    :: !(Map Name (Access, Context))
  , accessContextImports
    :: !(Map QName Context)
  } deriving Show

-- Prefer values from second context.
instance Semigroup Context where
  Context is1 ms1 <> Context is2 ms2
    = Context (is2 <> is1) (ms2 <> ms1)

-- Prefer values from second context.
instance Semigroup AccessContext where
  AccessContext is1 ms1 js1 <> AccessContext is2 ms2 js2
    = AccessContext (Map.unionWith (<>) is1 is2) (ms2 <> ms1) (js2 <> js1)

instance Monoid Context where
  mempty
    = Context mempty mempty

instance Monoid AccessContext where
  mempty
    = AccessContext mempty mempty mempty

-- Ensure that public names are not shadowed by private names.
accessModuleUnion
  :: (Access, Context)
  -> (Access, Context)
  -> (Access, Context)
accessModuleUnion c1@(Public, _) (Private, _)
  = c1
accessModuleUnion _ c2
  = c2

-- Ensure that public names are not shadowed by private names.
accessContextUnion
  :: AccessContext
  -> AccessContext
  -> AccessContext
accessContextUnion (AccessContext is1 ms1 js1) (AccessContext is2 ms2 js2)
  = AccessContext
  { accessContextItems
    = Map.unionWith accessItemUnion is1 is2
  , accessContextModules
    = Map.unionWith accessModuleUnion ms1 ms2
  , accessContextImports
    = js2 <> js1
  }

contextSingleton
  :: Name
  -> Item
  -> Context
contextSingleton n i
  = Context (Map.singleton n i) mempty

contextCons
  :: Name
  -> Context
  -> Context
contextCons n c
  = Context mempty (Map.singleton n c)

accessContextSingleton
  :: Name
  -> AccessItem
  -> AccessContext
accessContextSingleton n i
  = AccessContext (Map.singleton n i) mempty mempty

accessContextCons
  :: Name
  -> Access
  -> Context
  -> AccessContext
accessContextCons n a c
  = AccessContext mempty (Map.singleton n (a, c)) mempty

accessContextCons'
  :: Name
  -> Access
  -> AccessContext
  -> AccessContext
accessContextCons' n a c
  = accessContextCons n a (accessContextExport c)

accessContextImport
  :: QName
  -> Context
  -> AccessContext
accessContextImport n c
  = AccessContext mempty mempty (Map.singleton n c)

-- Do nothing if name not found.
contextInsert
  :: Name
  -> Range
  -> Context
  -> Context
contextInsert n r (Context is ms)
  = Context (Map.adjust (itemInsert r) n is) ms

contextInsertModule
  :: Name
  -> Range
  -> Context
  -> Context
contextInsertModule n r (Context is ms)
  = Context is (Map.adjust (contextInsertAll r) n ms)

contextInsertAll
  :: Range
  -> Context
  -> Context
contextInsertAll r (Context is ms)
  = Context (itemInsert r <$> is) (contextInsertAll r <$> ms)

contextRename
  :: Name
  -> Name
  -> Context
  -> Context
contextRename n n' (Context is ms)
  = Context (mapUpdateKey n n' is) ms

contextRenameModule
  :: Name
  -> Name
  -> Context
  -> Context
contextRenameModule n n' (Context is ms)
  = Context is (mapUpdateKey n n' ms)

contextDelete
  :: Name
  -> Context
  -> Context
contextDelete n (Context is ms)
  = Context (Map.delete n is) ms

contextDeleteModule
  :: Name
  -> Context
  -> Context
contextDeleteModule n (Context is ms)
  = Context is (Map.delete n ms)

resolve
  :: Maybe a
  -> Map k (Maybe a)
  -> Either LookupError a
resolve x xs
  = resolve' (catMaybes (x : Map.elems xs))

resolve'
  :: [a]
  -> Either LookupError a
resolve' []
  = Left LookupNotFound
resolve' (x : [])
  = Right x
resolve' (_ : _ : _)
  = Left LookupAmbiguous

contextLookup
  :: QName
  -> Context
  -> Maybe [Range]
contextLookup n c
  = itemRanges <$> contextLookupItem n c

contextLookupItem
  :: QName
  -> Context
  -> Maybe Item
contextLookupItem (QName n) (Context is _)
  = Map.lookup n is
contextLookupItem (Qual n ns) (Context _ ms)
  = Map.lookup n ms >>= contextLookupItem ns

contextLookupModule
  :: QName
  -> Context
  -> Maybe Context
contextLookupModule (QName n) (Context _ ms)
  = Map.lookup n ms
contextLookupModule (Qual n ns) (Context _ ms)
  = Map.lookup n ms >>= contextLookupModule ns

-- Return Nothing if we are currently defining an item with this name.
accessContextLookup
  :: QName
  -> AccessContext
  -> Either LookupError [Range]
accessContextLookup n c@(AccessContext _ _ is)
  = resolve
    (accessContextLookupMain n c)
    (Map.mapWithKey (accessContextLookupImport n) is)

accessContextLookupMain
  :: QName
  -> AccessContext
  -> Maybe [Range]
accessContextLookupMain n c
  = contextLookup n (toContext c)

accessContextLookupImport
  :: QName
  -- ^ The name to look up.
  -> QName
  -- ^ The name of an import.
  -> Context
  -- ^ The context of the import.
  -> Maybe [Range]
accessContextLookupImport n i c
  = stripPrefix i n >>= flip contextLookup c

accessContextLookupModule
  :: QName
  -> AccessContext
  -> Either LookupError Context
accessContextLookupModule n c@(AccessContext _ _ is)
  = resolve
    (accessContextLookupModuleMain n c)
    (Map.mapWithKey (accessContextLookupModuleImport n) is)

accessContextLookupModuleMain
  :: QName
  -> AccessContext
  -> Maybe Context
accessContextLookupModuleMain n c
  = contextLookupModule n (toContext c)

accessContextLookupModuleImport
  :: QName
  -- ^ The name to look up.
  -> QName
  -- ^ The name of an import.
  -> Context
  -- ^ The context of the import.
  -> Maybe Context
accessContextLookupModuleImport n i c | n == i
  = Just c
accessContextLookupModuleImport n i c
  = stripPrefix i n >>= flip contextLookupModule c

accessContextLookupName
  :: Name
  -> AccessContext
  -> [Range]
accessContextLookupName n (AccessContext is _ _)
  = maybe [] id (Map.lookup n is >>= accessItemRanges)

contextRanges
  :: Context
  -> [Range]
contextRanges (Context is ms)
  = concat (itemRanges <$> Map.elems is)
  <> concat (contextRanges <$> Map.elems ms)

accessContextOperators
  :: AccessContext
  -> [Name]
accessContextOperators (AccessContext is _ _)
  = filter isOperator (Map.keys is)

-- Return only constructors.
accessContextOperatorsP
  :: AccessContext
  -> [Name]
accessContextOperatorsP (AccessContext is _ _)
  = filter isOperator (Map.keys (Map.filter accessItemIsConstructor is))

fromContext
  :: Access
  -> Context
  -> AccessContext
fromContext a (Context is ms)
  = AccessContext (Map.map (fromItem a) is) (Map.map ((,) a) ms) mempty

toContext
  :: AccessContext
  -> Context
toContext (AccessContext is ms _)
  = Context (Map.map toItem is) (Map.map snd ms)

-- Clear imports from context.
accessContextClear
  :: AccessContext
  -> AccessContext
accessContextClear (AccessContext is ms _)
  = AccessContext is ms mempty

accessContextExport
  :: AccessContext
  -> Context
accessContextExport (AccessContext is ms _)
  = Context
  { contextItems
    = Map.mapMaybe accessItemExport is
  , contextModules
    = Map.mapMaybe (\(a, c) -> access Nothing (Just c) a) ms
  }

-- Make everything in context private.
accessContextPrivate
  :: AccessContext
  -> AccessContext
accessContextPrivate (AccessContext is ms js)
  = AccessContext
  { accessContextItems
    = Map.map accessItemPrivate is
  , accessContextModules
    = Map.map (\(_, c) -> (Private, c)) ms
  , accessContextImports
    = js
  }

-- Mark that we are currently defining an item with this name.
accessContextDefining
  :: Name
  -> AccessContext
  -> AccessContext
accessContextDefining n (AccessContext is ms js)
  = AccessContext (Map.adjust accessItemDefining n is) ms js

accessContextDefiningMay
  :: Maybe Name
  -> AccessContext
  -> AccessContext
accessContextDefiningMay n
  = maybe id accessContextDefining n

infixl 6 \\

(\\)
  :: AccessContext
  -> Maybe Name
  -> AccessContext
(\\)
  = flip accessContextDefiningMay

