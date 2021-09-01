module DataDef where

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

data D
  {y : ⊤}
  (y' : ⊤' y)
  : Set

data D {z} _ where

postulate

  d
    : D {tt} tt

