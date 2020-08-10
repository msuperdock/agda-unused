module PatternSyn where

data ⊤
  : Set
  where

  tt
    : ⊤

pattern p
  = tt

data D
  (A : Set)
  : Set
  where

  d
    : A
    → A
    → D A

pattern _,_ x y
  = d x y

f
  : ⊤
  → ⊤
f p
  = tt

g
  : {A : Set}
  → D A
  → A
g (x , _)
  = x

