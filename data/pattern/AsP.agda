module AsP where

f
  : {A : Set}
  → A
  → A
  → A
f x@y z@w
  = x

g
  : {A : Set}
  → A
  → A
  → A
g x@y z'@w'
  with y
... | _
  = x

