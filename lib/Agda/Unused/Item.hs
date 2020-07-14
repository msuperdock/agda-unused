module Agda.Unused.Item
  ( AccessItem (..)
  , Item (..)
  , accessItemDefining
  , accessItemExport
  , accessItemIsConstructor
  , accessItemPrivate
  , accessItemRanges
  , accessItemUnion
  , fromItem
  , itemInsert
  , itemRanges
  , toItem
  ) where

import Agda.Unused.Access
  (Access (..))
import Agda.Unused.Range
  (Range)

data Item where

  ItemConstructor
    :: ![Range]
    -> Item

  Item
    :: ![Range]
    -> Item

  deriving Show

data AccessItem where

  AccessItemConstructor
    :: ![Range]
    -- ^ Private ranges.
    -> ![Range]
    -- ^ Public ranges.
    -> AccessItem

  AccessItem
    :: !Access
    -> !Bool
    -- ^ Whether we are currently defining this item.
    -> ![Range]
    -> AccessItem

  deriving Show

-- Combine constructor ranges, otherwise return the second item.
instance Semigroup AccessItem where
  AccessItemConstructor rs1 ss1 <> AccessItemConstructor rs2 ss2
    = AccessItemConstructor (rs1 <> rs2) (ss1 <> ss2)
  _ <> i
    = i

-- Ensure that public names are not shadowed by private names.
accessItemUnion
  :: AccessItem
  -> AccessItem
  -> AccessItem
accessItemUnion i@(AccessItem Public _ _) (AccessItemConstructor _ [])
  = i
accessItemUnion i@(AccessItem Public _ _) (AccessItem Private _ _)
  = i
accessItemUnion i1 i2
  = i1 <> i2

itemInsert
  :: Range
  -> Item
  -> Item
itemInsert r (ItemConstructor rs)
  = ItemConstructor (r : rs)
itemInsert r (Item rs)
  = Item (r : rs)

accessItemDefining
  :: AccessItem
  -> AccessItem
accessItemDefining i@(AccessItemConstructor _ _)
  = i
accessItemDefining (AccessItem a _ rs)
  = AccessItem a True rs

accessItemIsConstructor
  :: AccessItem
  -> Bool
accessItemIsConstructor (AccessItemConstructor _ _)
  = True
accessItemIsConstructor _
  = False

itemRanges
  :: Item
  -> [Range]
itemRanges (ItemConstructor rs)
  = rs
itemRanges (Item rs)
  = rs

-- Return Nothing if defining.
accessItemRanges
  :: AccessItem
  -> Maybe [Range]
accessItemRanges (AccessItemConstructor rs ss)
  = Just (rs <> ss)
accessItemRanges (AccessItem _ False rs)
  = Just rs
accessItemRanges (AccessItem _ True _)
  = Nothing

fromItem
  :: Access
  -> Item
  -> AccessItem
fromItem Private (ItemConstructor rs)
  = AccessItemConstructor rs []
fromItem Public (ItemConstructor rs)
  = AccessItemConstructor [] rs
fromItem a (Item rs)
  = AccessItem a False rs
 
toItem
  :: AccessItem
  -> Item
toItem (AccessItemConstructor rs ss)
  = ItemConstructor (rs <> ss)
toItem (AccessItem _ _ rs)
  = Item rs

accessItemExport
  :: AccessItem
  -> Maybe Item
accessItemExport (AccessItemConstructor _ rs)
  = Just (ItemConstructor rs)
accessItemExport (AccessItem Private _ _)
  = Nothing
accessItemExport (AccessItem Public _ rs)
  = Just (Item rs)

accessItemPrivate
  :: AccessItem
  -> AccessItem
accessItemPrivate (AccessItemConstructor rs ss)
  = AccessItemConstructor (rs <> ss) []
accessItemPrivate (AccessItem _ b rs)
  = AccessItem Private b rs

