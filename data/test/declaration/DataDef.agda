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
  {x : ⊤}
  (y : ⊤' x)
  : Set

data D {x} _ where

