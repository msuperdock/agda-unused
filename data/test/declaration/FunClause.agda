module FunClause where

f
  : {A : Set}
  → A
  → A
f x
  = y
  where
    y = x
    z = x

data List
  (A : Set)
  : Set
  where

  nil
    : List A

  cons
    : A
    → List A
    → List A

snoc
  : {A : Set}
  → List A
  → A
  → List A
snoc nil y
  = cons y nil
snoc (cons x xs) y
  = cons x (snoc xs y)

