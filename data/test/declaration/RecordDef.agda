module RecordDef where

data ⊤
  : Set
  where

  tt
    : ⊤

data ⊤'
  (x : ⊤)
  : Set
  where

  tt
    : ⊤' x

record R
  {x : ⊤}
  (y : ⊤' x)
  : Set

record R {x} _ where

