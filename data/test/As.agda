module As where

f
  : {A : Set}
  → A
  → A
f x@y
  with y
... | _
  = x

