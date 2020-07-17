module With2 where

f
  : {A : Set}
  → A
  → A
  → A
f x y
  with x
... | _
  with y
... | _
  = x

