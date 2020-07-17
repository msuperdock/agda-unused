module Agda.Unused.Monad.Context.Item
  ( AccessItem
  , Item (..)
  , accessItem
  , accessItemConstructor
  , accessItemDefining
  , accessItemExport
  , accessItemHasRange
  , accessItemIsConstructor
  , accessItemPrivate
  , accessItemRangesMay
  , accessItemUnion
  , fromItem
  , itemHasRange
  , itemInsert
  , itemRanges
  , toItem
  ) where

import Agda.Unused.Monad.Reader
  (Environment, askBuiltin)
import Agda.Unused.Types.Access
  (Access (..))
import Agda.Unused.Types.Range
  (Range)

import Control.Monad.Reader
  (MonadReader)
import Data.Bool
  (bool)

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

-- Use empty range if in a builtin module.
accessItem
  :: MonadReader Environment m
  => Access
  -> [Range]
  -> m AccessItem
accessItem a rs
  = askBuiltin >>= \b -> pure (AccessItem a False (bool rs [] b))

-- Use empty ranges if in a builtin module.
accessItemConstructor
  :: MonadReader Environment m
  => Access
  -> [Range]
  -> m AccessItem
accessItemConstructor Private rs
  = askBuiltin >>= \b -> pure (AccessItemConstructor (bool rs [] b) [])
accessItemConstructor Public rs
  = askBuiltin >>= \b -> pure (AccessItemConstructor [] (bool rs [] b))

itemInsert
  :: MonadReader Environment m
  => Range
  -> Item
  -> m Item
itemInsert r (ItemConstructor rs)
  = askBuiltin >>= pure . ItemConstructor . bool (r : rs) []
itemInsert r (Item rs)
  = askBuiltin >>= pure . Item . bool (r : rs) []

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

accessItemRanges
  :: AccessItem
  -> [Range]
accessItemRanges (AccessItemConstructor rs ss)
  = rs <> ss
accessItemRanges (AccessItem _ _ rs)
  = rs

-- Return Nothing if defining.
accessItemRangesMay
  :: AccessItem
  -> Maybe [Range]
accessItemRangesMay (AccessItem _ True _)
  = Nothing
accessItemRangesMay i
  = Just (accessItemRanges i)

itemHasRange
  :: Range
  -> Item
  -> Bool
itemHasRange r
  = elem r . itemRanges

accessItemHasRange
  :: Range
  -> AccessItem
  -> Bool
accessItemHasRange r
  = elem r . accessItemRanges

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

