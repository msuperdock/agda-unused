module Let where

f
  : {A : Set}
  → A
  → A
f x
  = let
      y = x
      z = x
    in y

