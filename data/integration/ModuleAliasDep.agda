module ModuleAliasDep where

data T
  : Set
  where

  c
    : T

  d
    : T

record Apex
  (A : Set)
  : Set
  where

  field

    psi
      : A
