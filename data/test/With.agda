module With where

f
  : {A : Set}
  → A
  → A
f x
  with x
... | y
  = y

