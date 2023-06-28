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

record Id
  (A : Set)
  : Set
  where

  constructor

    id

  field

    value
      : A

g
  : {A : Set}
  → Id A
  → A
g x'
  = let id y' = x'
    in let id z' = x'
    in y'

