module ExtendedLam where

data Bool
  : Set
  where

  false
    : Bool
  true
    : Bool

f
  : Bool
  → Bool
  → Bool
f
  = λ
  { x false
    → true
  ; y true
    → y
  }

