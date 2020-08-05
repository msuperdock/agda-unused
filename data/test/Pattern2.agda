module Pattern2 where

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
  : {A : Set}
  → D A
  → A
f (x , _)
  = x

