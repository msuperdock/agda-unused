module OpAppP where

data Bool
  : Set
  where

  false
    : Bool

  true
    : Bool

_&&_
  : Bool
  → Bool
  → Bool
false && _
  = false
true && b
  = b

