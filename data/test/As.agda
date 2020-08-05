module As where

f
  : {A : Set}
  → A
  → A
  → A
f x@y z@w
  with y
... | _
  = x

