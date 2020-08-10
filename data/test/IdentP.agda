module IdentP where

data Bool
  : Set
  where

  false
    : Bool

  true
    : Bool

f
  : {A : Set}
  → A
  → A
  → A
f x y
  = x

g
  : Bool
  → Bool
g false
  = true
g true
  = true

