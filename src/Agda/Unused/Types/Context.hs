{- |
Module: Agda.Unused.Types.Context

Definitions and interface for the 'Context' and 'AccessContext' types, which
represent namespaces of definitions.
-}
module Agda.Unused.Types.Context

  ( -- * Definitions

    Item
  , Module(Module)
  , AccessModule(AccessModule)
  , Context
  , AccessContext
  , accessContextUnion

    -- * Interface

    -- ** Lookup

  , LookupError(..)
  , contextLookup
  , contextLookupItem
  , contextLookupModule
  , accessContextLookup
  , accessContextLookupModule
  , accessContextLookupDefining
  , accessContextLookupSpecial
  
    -- ** Insert

  , contextInsertRangeAll
  , accessContextInsertRangeAll

    -- ** Delete

  , contextDelete
  , contextDeleteModule

    -- ** Define

  , accessContextDefine

    -- ** Ranges

  , moduleRanges
  , contextRanges

    -- ** Match

  , accessContextMatch

    -- * Construction

  , item
  , itemPattern
  , itemConstructor
  , contextItem
  , contextModule
  , accessContextItem
  , accessContextModule
  , accessContextModule'
  , accessContextImport

    -- * Conversion

  , fromContext
  , toContext

  ) where

import Agda.Unused.Types.Access
  (Access(..))
import Agda.Unused.Types.Name
  (Name, QName(..), matchOperators, stripPrefix)
import Agda.Unused.Types.Range
  (Range)

import Data.Map.Strict
  (Map)
import qualified Data.Map.Strict
  as Map
import Data.Maybe
  (catMaybes)

-- ## Definitions

-- | The data associated with a name in context. This includes:
--
-- - Whether the name is a constructor, pattern synonym, or ordinary definition.
-- - A list of ranges associated with the name, which includes the site of the
-- original definition, as well as any relevant @import@ or @open@ statements.
-- - Alternative syntax for the name, if any.
data Item where

  ItemConstructor
    :: ![Range]
    -> ![Name]
    -> Item

  ItemPattern
    :: ![Range]
    -> !(Maybe Name)
    -> Item

  Item
    :: ![Range]
    -> !(Maybe Name)
    -> Item

  deriving Show

-- Like 'Item', but with some additional data:
--
-- - Whether the name is public or private.
-- - Whether the name is currently being defined.
--
-- Since constructors may be overloaded, a constructor AccessItem may
-- represent multiple constructors, some public and some private.
data AccessItem where

  AccessItemConstructor
    -- Private ranges.
    :: ![Range]
    -- Public ranges.
    -> ![Range]
    -- Private syntax.
    -> ![Name]
    -- Public syntax.
    -> ![Name]
    -> AccessItem

  AccessItemPattern
    :: !Access
    -> ![Range]
    -> !(Maybe Name)
    -> AccessItem

  AccessItemSyntax
    -- Whether the item is special.
    :: !Bool
    -> ![Range]
    -> AccessItem

  AccessItem
    -- Whether we are currently defining this item.
    :: !Bool
    -> !Access
    -> ![Range]
    -> !(Maybe Name)
    -> AccessItem

  deriving Show

-- | The data associated with a module in context. This includes:
--
-- - A list of ranges associated with the module, which includes the site of the
-- original definition, as well as any relevant @import@ or @open@ statements.
-- - The inner context of the module.
data Module
  = Module
  { moduleRanges'
    :: ![Range]
  , moduleContext
    :: !Context
  } deriving Show

-- | Like 'Module', but also recording whether the module is public or private.
data AccessModule
  = AccessModule
  { accessModuleAccess
    :: !Access
  , accessModuleRanges
    :: ![Range]
  , accessModuleContext
    :: !Context
  } deriving Show

-- | A namespace of definitions. Any Agda module produces a 'Context'.
data Context
  = Context
  { contextItems
    :: !(Map Name Item)
  , contextModules
    :: !(Map Name Module)
  } deriving Show

-- | A namespace of definitions, which may be public or private. Any collection
-- of Agda declarations produces an 'AccessContext', for example.
data AccessContext
  = AccessContext
  { accessContextItems
    :: !(Map Name AccessItem)
  , accessContextModules
    :: !(Map Name AccessModule)
  , accessContextImports
    :: !(Map QName Context)
  } deriving Show

-- | If both items are constructors, collect the private and public ranges for
-- both. Otherwise, return the second item.
instance Semigroup AccessItem where
  AccessItemConstructor rs1 ss1 ts1 us1 <> AccessItemConstructor rs2 ss2 ts2 us2
    = AccessItemConstructor (rs1 <> rs2) (ss1 <> ss2) (ts1 <> ts2) (us1 <> us2)
  _ <> i
    = i

-- | Prefer values from second context.
instance Semigroup Context where
  Context is1 ms1 <> Context is2 ms2
    = Context (is2 <> is1) (ms2 <> ms1)

-- | Prefer values from second access context.
instance Semigroup AccessContext where
  AccessContext is1 ms1 js1 <> AccessContext is2 ms2 js2
    = AccessContext (Map.unionWith (<>) is1 is2) (ms2 <> ms1) (js2 <> js1)

instance Monoid Context where
  mempty
    = Context mempty mempty

instance Monoid AccessContext where
  mempty
    = AccessContext mempty mempty mempty

-- Ensure public names are not shadowed by private names.
accessItemUnion
  :: AccessItem
  -> AccessItem
  -> AccessItem
accessItemUnion i@(AccessItem _ Public _ _) (AccessItemConstructor _ [] _ _)
  = i
accessItemUnion i@(AccessItem _ Public _ _) (AccessItem _ Private _ _)
  = i
accessItemUnion i1 i2
  = i1 <> i2

-- Ensure public names are not shadowed by private names.
accessModuleUnion
  :: AccessModule
  -> AccessModule
  -> AccessModule
accessModuleUnion m1@(AccessModule Public _ _) (AccessModule Private _ _)
  = m1
accessModuleUnion _ m2
  = m2

-- | Like '(<>)', but public items take precedence over private items. This is
-- important when combining contexts from successive declarations; for example:
--
-- @ 
-- module M where
--
--   postulate
--     A : Set
--
-- module N where
--
--   postulate
--     A : Set
--
--   open M
--
-- x : N.A
-- x = ?
-- @ 
--
-- This code type-checks, and the identifier @N.A@ refers to the postulate
-- declared in the definition of @N@, not the definition opened from @M@.
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

-- ## Interface

-- ### Lookup

-- | A description of failure for an 'AccessContext' lookup.
data LookupError where

  LookupNotFound
    :: LookupError

  LookupAmbiguous
    :: LookupError

  deriving Show

-- | Get the ranges for the given name, or 'Nothing' if not in context.
contextLookup
  :: QName
  -> Context
  -> Maybe [Range]
contextLookup n c
  = itemRanges <$> contextLookupItem n c

-- | Get the inner module for the given name, or 'Nothing' if not in context.
contextLookupModule
  :: QName
  -> Context
  -> Maybe Module
contextLookupModule (QName n) (Context _ ms)
  = Map.lookup n ms
contextLookupModule (Qual n ns) (Context _ ms)
  = Map.lookup n ms >>= contextLookupModule ns . moduleContext

-- | Get the item for the given name, or 'Nothing' if not in context.
contextLookupItem
  :: QName
  -> Context
  -> Maybe Item
contextLookupItem (QName n) (Context is _)
  = Map.lookup n is
contextLookupItem (Qual n ns) (Context _ ms)
  = Map.lookup n ms >>= contextLookupItem ns . moduleContext

-- | Get the ranges for the given name, or produce a 'LookupError'.
accessContextLookup
  :: QName
  -> AccessContext
  -> Either LookupError [Range]
accessContextLookup n c@(AccessContext _ _ is)
  = contextLookup n (toContext' c)
  <|> Map.mapWithKey (accessContextLookupImport n) is

-- | Get the inner module for the given name, or produce a 'LookupError'.
accessContextLookupModule
  :: QName
  -> AccessContext
  -> Either LookupError Module
accessContextLookupModule n c@(AccessContext _ _ is)
  = contextLookupModule n (toContext' c)
  <|> Map.mapWithKey (accessContextLookupModuleImport n) is

accessContextLookupImport
  :: QName
  -> QName
  -> Context
  -> Maybe [Range]
accessContextLookupImport n i c
  = stripPrefix i n >>= flip contextLookup c

accessContextLookupModuleImport
  :: QName
  -> QName
  -> Context
  -> Maybe Module
accessContextLookupModuleImport n i c | n == i
  = Just (Module [] c)
accessContextLookupModuleImport n i c
  = stripPrefix i n >>= flip contextLookupModule c

(<|>)
  :: Maybe a
  -> Map k (Maybe a)
  -> Either LookupError a
x <|> xs
  = resolve (catMaybes (x : Map.elems xs))

resolve
  :: [a]
  -> Either LookupError a
resolve []
  = Left LookupNotFound
resolve (x : [])
  = Right x
resolve (_ : _ : _)
  = Left LookupAmbiguous

accessItemDefining
  :: AccessItem
  -> Bool
accessItemDefining (AccessItem b _ _ _)
  = b
accessItemDefining _
  = False

-- | Like 'accessContextLookup', but also return a boolean indicating whether we
-- are currently defining the referenced item.
accessContextLookupDefining
  :: QName
  -> AccessContext
  -> Either LookupError (Bool, [Range])
accessContextLookupDefining (QName n) (AccessContext is _ _)
  = maybe
    (Left LookupNotFound)
    (\i -> Right (accessItemDefining i, accessItemRanges i))
    (Map.lookup n is)
accessContextLookupDefining n@(Qual _ _) c
  = (,) False <$> accessContextLookup n c

itemSpecial
  :: Item
  -> Bool
itemSpecial (ItemConstructor _ _)
  = True
itemSpecial (ItemPattern _ _)
  = True
itemSpecial (Item _ _)
  = False

-- | Determine whether a name represents a constructor or pattern synonym.
-- Return 'Nothing' if the name is not in context.
accessContextLookupSpecial
  :: QName
  -> AccessContext
  -> Maybe Bool
accessContextLookupSpecial n c
  = itemSpecial <$> contextLookupItem n (toContext' c)

-- ### Insert

itemInsertRange
  :: Range
  -> Item
  -> Item
itemInsertRange r (ItemConstructor rs ss)
  = ItemConstructor (r : rs) ss
itemInsertRange r (ItemPattern rs s)
  = ItemPattern (r : rs) s
itemInsertRange r (Item rs s)
  = Item (r : rs) s

accessItemInsertRange
  :: Range
  -> AccessItem
  -> AccessItem
accessItemInsertRange r (AccessItemConstructor rs1 rs2 ns1 ns2)
  = AccessItemConstructor (r : rs1) (r : rs2) ns1 ns2
accessItemInsertRange r (AccessItemPattern a rs n)
  = AccessItemPattern a (r : rs) n
accessItemInsertRange r (AccessItemSyntax b rs)
  = AccessItemSyntax b (r : rs)
accessItemInsertRange r (AccessItem b a rs n)
  = AccessItem b a (r : rs) n

moduleInsertRangeAll
  :: Range
  -> Module
  -> Module
moduleInsertRangeAll r (Module rs c)
  = Module (r : rs) (contextInsertRangeAll r c)

accessModuleInsertRangeAll
  :: Range
  -> AccessModule
  -> AccessModule
accessModuleInsertRangeAll r (AccessModule a rs c)
  = AccessModule a (r : rs) (contextInsertRangeAll r c)

-- | Insert a range for all names in a context.
contextInsertRangeAll
  :: Range
  -> Context
  -> Context
contextInsertRangeAll r (Context is ms)
  = Context
    (itemInsertRange r <$> is)
    (moduleInsertRangeAll r <$> ms)

-- | Insert a range for all names in an access context.
accessContextInsertRangeAll
  :: Range
  -> AccessContext
  -> AccessContext
accessContextInsertRangeAll r (AccessContext is ms js)
  = AccessContext
    (accessItemInsertRange r <$> is)
    (accessModuleInsertRangeAll r <$> ms) js

-- ### Delete

-- | Delete an item from the context.
contextDelete
  :: Name
  -> Context
  -> Context
contextDelete n (Context is ms)
  = Context (Map.delete n is) ms

-- | Delete a module from the context.
contextDeleteModule
  :: Name
  -> Context
  -> Context
contextDeleteModule n (Context is ms)
  = Context is (Map.delete n ms)

-- ### Define

accessItemDefine
  :: AccessItem
  -> AccessItem
accessItemDefine (AccessItem _ a rs s)
  = AccessItem True a rs s
accessItemDefine i
  = i

-- | Mark an existing name as in process of being defined.
accessContextDefine
  :: Name
  -> AccessContext
  -> AccessContext
accessContextDefine n (AccessContext is ms js)
  = AccessContext (Map.adjust accessItemDefine n is) ms js

-- ### Ranges

itemRanges
  :: Item
  -> [Range]
itemRanges (ItemConstructor rs _)
  = rs
itemRanges (ItemPattern rs _)
  = rs
itemRanges (Item rs _)
  = rs

accessItemRanges
  :: AccessItem
  -> [Range]
accessItemRanges
  = itemRanges . toItem'

-- | Get all ranges associated with names in the given module, including ranges
-- associated with the module itself.
moduleRanges
  :: Module
  -> [Range]
moduleRanges (Module rs c)
  = rs <> contextRanges c

-- | Get all ranges associated with names in the given context.
contextRanges
  :: Context
  -> [Range]
contextRanges (Context is ms)
  = concat (itemRanges <$> Map.elems is)
  <> concat (moduleRanges <$> Map.elems ms)

-- ### Match

-- | Find all operators matching the given list of tokens.
accessContextMatch
  :: [String]
  -> AccessContext
  -> [Name]
accessContextMatch ss (AccessContext is _ _)
  = matchOperators ss (Map.keys is)

-- ## Construction

-- | Construct an 'Item' representing an ordinary definition.
item
  :: [Range]
  -> Maybe Name
  -> Item
item
  = Item

-- | Construct an 'Item' representing a pattern synonym.
itemPattern
  :: [Range]
  -> Maybe Name
  -> Item
itemPattern
  = ItemPattern

-- | Construct an 'Item' representing a constructor.
itemConstructor
  :: [Range]
  -> Maybe Name
  -> Item
itemConstructor rs Nothing
  = ItemConstructor rs []
itemConstructor rs (Just s)
  = ItemConstructor rs [s]

-- | Construct a 'Context' with a single item.
contextItem
  :: Name
  -> Item
  -> Context
contextItem n i
  = Context (Map.singleton n i) mempty

-- | Construct a 'Context' with a single module.
contextModule
  :: Name
  -> Module
  -> Context
contextModule n m
  = Context mempty (Map.singleton n m)

-- | Construct an 'AccessContext' with a single item, along with the relevant
-- syntax item if applicable.
accessContextItem
  :: Name
  -> Access
  -> Item
  -> AccessContext
accessContextItem n a i
  = fromContext a (contextItem n i)

-- | Construct an 'AccessContext' with a single access module.
accessContextModule
  :: Name
  -> AccessModule
  -> AccessContext
accessContextModule n m
  = AccessContext mempty (Map.singleton n m) mempty

-- | Like 'accessContextModule', but taking an access context. We convert the
-- given access context to an ordinary context using 'toContext':
--
-- @
-- accessContextModule' n a rs c
--   = accessContextModule n (AccessModule a rs (toContext c))
-- @
accessContextModule'
  :: Name
  -> Access
  -> [Range]
  -> AccessContext
  -> AccessContext
accessContextModule' n a rs c
  = accessContextModule n (AccessModule a rs (toContext c))

-- | Construct an access context with a single import.
accessContextImport
  :: QName
  -> Context
  -> AccessContext
accessContextImport n c
  = AccessContext mempty mempty (Map.singleton n c)

-- ## Conversion

fromItem
  :: Access
  -> Item
  -> AccessItem
fromItem Private (ItemConstructor rs ss)
  = AccessItemConstructor rs [] ss []
fromItem Public (ItemConstructor rs ss)
  = AccessItemConstructor [] rs [] ss
fromItem a (ItemPattern rs s)
  = AccessItemPattern a rs s
fromItem a (Item rs s)
  = AccessItem False a rs s

fromItemSyntax
  :: Item
  -> [(Name, AccessItem)]
fromItemSyntax (ItemConstructor rs ss)
  = flip (,) (AccessItemSyntax True rs) <$> ss
fromItemSyntax (ItemPattern rs s)
  = flip (,) (AccessItemSyntax True rs) <$> maybe [] (: []) s
fromItemSyntax (Item rs s)
  = flip (,) (AccessItemSyntax False rs) <$> maybe [] (: []) s

toItem
  :: AccessItem
  -> Maybe Item
toItem (AccessItemConstructor _ rs@(_ : _) _ ss)
  = Just (ItemConstructor rs ss)
toItem (AccessItemPattern Public rs s)
  = Just (ItemPattern rs s)
toItem (AccessItem _ Public rs s)
  = Just (Item rs s)
toItem _
  = Nothing

toItem'
  :: AccessItem
  -> Item
toItem' (AccessItemConstructor rs1 rs2 ss1 ss2)
  = ItemConstructor (rs1 <> rs2) (ss1 <> ss2)
toItem' (AccessItemPattern _ rs s)
  = ItemPattern rs s
toItem' (AccessItemSyntax _ rs)
  = Item rs Nothing
toItem' (AccessItem _ _ rs s)
  = Item rs s

fromModule
  :: Access
  -> Module
  -> AccessModule
fromModule a (Module rs c)
  = AccessModule a rs c

toModule
  :: AccessModule
  -> Maybe Module
toModule (AccessModule Private _ _)
  = Nothing
toModule (AccessModule Public rs c)
  = Just (Module rs c)

toModule'
  :: AccessModule
  -> Module
toModule' (AccessModule _ rs c)
  = Module rs c

-- | Convert a 'Context' to 'AccessContext'. Give all items the given access.
fromContext
  :: Access
  -> Context
  -> AccessContext
fromContext a (Context is ms)
  = AccessContext
    (Map.map (fromItem a) is <> Map.fromList (Map.elems is >>= fromItemSyntax))
    (Map.map (fromModule a) ms)
    mempty

-- | Convert an 'AccessContext' to 'Context'. Discard private items and imports.
toContext
  :: AccessContext
  -> Context
toContext (AccessContext is ms _)
  = Context (Map.mapMaybe toItem is) (Map.mapMaybe toModule ms)

-- Like 'toContext`, but keep private items.
toContext'
  :: AccessContext
  -> Context
toContext' (AccessContext is ms _)
  = Context (Map.map toItem' is) (Map.map toModule' ms)

