module Pattern where

data ⊤
  : Set
  where

  tt
    : ⊤

pattern p
  = tt

f
  : ⊤
  → ⊤
f p
  = tt

