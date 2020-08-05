module Constructor where

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
f false
  = true
f true
  = true

