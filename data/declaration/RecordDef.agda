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
  {y : ⊤}
  (y' : ⊤' y)
  : Set

record R {z} _ where

postulate

  r
    : R {tt} tt

