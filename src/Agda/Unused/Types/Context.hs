{- |
Module : Agda.Unused.Types.Context

Definitions and interface for the 'Context' and 'AccessContext' types, which
represent namespaces of definitions.
-}
module Agda.Unused.Types.Context

  ( -- * Definitions

    Item
  , AccessItem
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
  , accessContextLookupSpecial
  
    -- ** Insert

  , contextInsertRange
  , contextInsertRangeModule
  , contextInsertRangeAll

    -- ** Delete

  , contextDelete
  , contextDeleteModule

    -- ** Rename

  , contextRename
  , contextRenameModule

    -- ** Defining

  , accessContextDefining

    -- ** Ranges

  , contextRanges

    -- ** Match

  , accessContextMatch
  , accessContextMatchPattern

    -- * Construction

  , accessItem
  , accessItemConstructor
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
import Agda.Unused.Utils
  (mapUpdateKey)

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
-- original definition, as well as any relevant "import" or "open" statements.
data Item where

  ItemConstructor
    :: ![Range]
    -> Item

  ItemPattern
    :: ![Range]
    -> Item

  Item
    :: ![Range]
    -> Item

  deriving Show

-- | Like 'Item', but with some additional data:
--
-- - Whether the name is public or private.
-- - Whether the name is currently being defined.
--
-- Since constructors may be overloaded, a constructor 'AccessItem' may
-- represent multiple constructors, some public and some private.
data AccessItem where

  AccessItemConstructor
    -- Private ranges.
    :: ![Range]
    -- Public ranges.
    -> ![Range]
    -> AccessItem

  AccessItemPattern
    :: !Access
    -> ![Range]
    -> AccessItem

  AccessItemDefining
    :: !Access
    -> ![Range]
    -> AccessItem

  AccessItem
    :: !Access
    -> ![Range]
    -> AccessItem

  deriving Show

-- | A namespace of definitions. Any Agda module produces a 'Context'.
data Context
  = Context
  { contextItems
    :: !(Map Name Item)
  , contextModules
    :: !(Map Name Context)
  } deriving Show

-- | A namespace of definitions, which may be public or private. Any collection
-- of Agda declarations produces an 'AccessContext', for example.
data AccessContext
  = AccessContext
  { accessContextItems
    :: !(Map Name AccessItem)
  , accessContextModules
    :: !(Map Name (Access, Context))
  , accessContextImports
    :: !(Map QName Context)
  } deriving Show

-- | If both items are constructors, collect the private and public ranges for
-- both. Otherwise, return the second item.
instance Semigroup AccessItem where
  AccessItemConstructor rs1 ss1 <> AccessItemConstructor rs2 ss2
    = AccessItemConstructor (rs1 <> rs2) (ss1 <> ss2)
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
accessItemUnion i@(AccessItem Public _) (AccessItemConstructor _ [])
  = i
accessItemUnion i@(AccessItem Public _) (AccessItem Private _)
  = i
accessItemUnion i1 i2
  = i1 <> i2

-- Ensure public names are not shadowed by private names.
accessModuleUnion
  :: (Access, Context)
  -> (Access, Context)
  -> (Access, Context)
accessModuleUnion c1@(Public, _) (Private, _)
  = c1
accessModuleUnion _ c2
  = c2

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
-- This code type-checks, and the identifier N.A refers to the postulate
-- declared in the definition of N, not the definition opened from M.
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

-- | Get the inner context for the given name, or 'Nothing' if not in context.
contextLookupModule
  :: QName
  -> Context
  -> Maybe Context
contextLookupModule (QName n) (Context _ ms)
  = Map.lookup n ms
contextLookupModule (Qual n ns) (Context _ ms)
  = Map.lookup n ms >>= contextLookupModule ns

-- | Get the item for the given name, or 'Nothing' if not in context.
contextLookupItem
  :: QName
  -> Context
  -> Maybe Item
contextLookupItem (QName n) (Context is _)
  = Map.lookup n is
contextLookupItem (Qual n ns) (Context _ ms)
  = Map.lookup n ms >>= contextLookupItem ns

-- | Get the ranges for the given name, or produce a 'LookupError'.
accessContextLookup
  :: QName
  -> AccessContext
  -> Either LookupError [Range]
accessContextLookup n c@(AccessContext _ _ is)
  = contextLookup n (toContext' c)
  <|> Map.mapWithKey (accessContextLookupImport n) is

-- | Get the inner context for the given name, or produce a 'LookupError'.
accessContextLookupModule
  :: QName
  -> AccessContext
  -> Either LookupError Context
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
  -> Maybe Context
accessContextLookupModuleImport n i c | n == i
  = Just c
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

itemSpecial
  :: Item
  -> Bool
itemSpecial (ItemConstructor _)
  = True
itemSpecial (ItemPattern _)
  = True
itemSpecial (Item _)
  = False

accessItemSpecial
  :: AccessItem
  -> Bool
accessItemSpecial
  = itemSpecial . toItem'

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
itemInsertRange r (ItemConstructor rs)
  = ItemConstructor (r : rs)
itemInsertRange r (ItemPattern rs)
  = ItemPattern (r : rs)
itemInsertRange r (Item rs)
  = Item (r : rs)

-- | Insert a range for the given name, if present.
contextInsertRange
  :: Name
  -> Range
  -> Context
  -> Context
contextInsertRange n r (Context is ms)
  = Context (Map.adjust (itemInsertRange r) n is) ms

-- | Insert a range for all names in the given module, if present.
contextInsertRangeModule
  :: Name
  -> Range
  -> Context
  -> Context
contextInsertRangeModule n r (Context is ms)
  = Context is (Map.adjust (contextInsertRangeAll r) n ms)

-- | Insert a range for all names in context.
contextInsertRangeAll
  :: Range
  -> Context
  -> Context
contextInsertRangeAll r (Context is ms)
  = Context (itemInsertRange r <$> is) (contextInsertRangeAll r <$> ms)

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

-- ### Rename

-- | Rename an item, if present.
contextRename
  :: Name
  -> Name
  -> Context
  -> Context
contextRename n n' (Context is ms)
  = Context (mapUpdateKey n n' is) ms

-- | Rename a module, if present.
contextRenameModule
  :: Name
  -> Name
  -> Context
  -> Context
contextRenameModule n n' (Context is ms)
  = Context is (mapUpdateKey n n' ms)

-- ### Defining

accessItemDefining
  :: AccessItem
  -> AccessItem
accessItemDefining (AccessItem a rs)
  = AccessItemDefining a rs
accessItemDefining i
  = i

-- | Mark an existing name as in process of being defined.
accessContextDefining
  :: Name
  -> AccessContext
  -> AccessContext
accessContextDefining n (AccessContext is ms js)
  = AccessContext (Map.adjust accessItemDefining n is) ms js

-- ### Ranges

-- Get all ranges associated with the given item.
itemRanges
  :: Item
  -> [Range]
itemRanges (ItemConstructor rs)
  = rs
itemRanges (ItemPattern rs)
  = rs
itemRanges (Item rs)
  = rs

-- | Get all ranges associated with names in the given context.
contextRanges
  :: Context
  -> [Range]
contextRanges (Context is ms)
  = concat (itemRanges <$> Map.elems is)
  <> concat (contextRanges <$> Map.elems ms)

-- ### Match

-- | Find all operators matching the given list of tokens.
accessContextMatch
  :: [String]
  -> AccessContext
  -> [Name]
accessContextMatch ss (AccessContext is _ _)
  = matchOperators ss (Map.keys is)

-- | Like 'accessContextMatch', restricted to constructors or pattern synonyms.
accessContextMatchPattern
  :: [String]
  -> AccessContext
  -> [Name]
accessContextMatchPattern ss (AccessContext is _ _)
  = matchOperators ss (Map.keys (Map.filter accessItemSpecial is))

-- ## Construction

-- data AccessItem where

--   AccessItemConstructor
--     :: ![Range]
--     -- ^ Private ranges.
--     -> ![Range]
--     -- ^ Public ranges.
--     -> AccessItem

--   AccessItemPattern
--     :: !Access
--     -> ![Range]
--     -> AccessItem

--   AccessItemDefining
--     :: !Access
--     -> ![Range]
--     -> AccessItem

--   AccessItem
--     :: !Access
--     -> ![Range]
--     -> AccessItem

-- | Construct an 'AccessItem' representing an ordinary definition.
accessItem
  :: Access
  -> [Range]
  -> AccessItem
accessItem
  = AccessItem

-- | Construct an 'AccessItem' representing a constructor.
accessItemConstructor
  :: Access
  -> [Range]
  -> AccessItem
accessItemConstructor Private rs
  = AccessItemConstructor rs []
accessItemConstructor Public rs
  = AccessItemConstructor [] rs

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
  -> Context
  -> Context
contextModule n c
  = Context mempty (Map.singleton n c)

-- | Construct an 'AccessContext' with a single item.
accessContextItem
  :: Name
  -> AccessItem
  -> AccessContext
accessContextItem n i
  = AccessContext (Map.singleton n i) mempty mempty

-- | Construct an 'AccessContext' with a single module.
accessContextModule
  :: Name
  -> Access
  -> Context
  -> AccessContext
accessContextModule n a c
  = AccessContext mempty (Map.singleton n (a, c)) mempty

-- | Like 'accessContextModule', but first convert the given access context to
-- an ordinary context using 'toContext':
--
-- @
-- accessContextModule' n a
--   = accessContextModule n a . toContext
-- @
accessContextModule'
  :: Name
  -> Access
  -> AccessContext
  -> AccessContext
accessContextModule' n a
  = accessContextModule n a . toContext

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
fromItem Private (ItemConstructor rs)
  = AccessItemConstructor rs []
fromItem Public (ItemConstructor rs)
  = AccessItemConstructor [] rs
fromItem a (ItemPattern rs)
  = AccessItemPattern a rs
fromItem a (Item rs)
  = AccessItem a rs

toItem
  :: AccessItem
  -> Maybe Item
toItem (AccessItemConstructor _ rs)
  = Just (ItemConstructor rs)
toItem (AccessItemPattern Public rs)
  = Just (ItemPattern rs)
toItem (AccessItem Public rs)
  = Just (Item rs)
toItem _
  = Nothing

toItem'
  :: AccessItem
  -> Item
toItem' (AccessItemConstructor rs1 rs2)
  = ItemConstructor (rs1 <> rs2)
toItem' (AccessItemPattern _ rs)
  = ItemPattern rs
toItem' (AccessItemDefining _ rs)
  = Item rs
toItem' (AccessItem _ rs)
  = Item rs

toModule
  :: (Access, Context)
  -> Maybe Context
toModule (Private, _)
  = Nothing
toModule (Public, c)
  = Just c

-- | Convert a 'Context' to 'AccessContext'. Give all items the given access.
fromContext
  :: Access
  -> Context
  -> AccessContext
fromContext a (Context is ms)
  = AccessContext (Map.map (fromItem a) is) (Map.map ((,) a) ms) mempty

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
  = Context (Map.map toItem' is) (Map.map snd ms)

