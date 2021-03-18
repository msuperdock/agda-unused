module WithApp where

f
  : {A : Set}
  → A
  → A
f x
  with x
... | y
  = y

g
  : {A : Set}
  → A
  → A
  → A
g x y
  with x
... | _
  with y
... | _
  = x

