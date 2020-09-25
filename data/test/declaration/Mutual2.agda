module Mutual2 where

data Bool
  : Set
  where

  false
    : Bool

  true
    : Bool

data ℕ
  : Set
  where

  zero
    : ℕ

  suc
    : ℕ
    → ℕ

is-even
  : ℕ
  → Bool

is-odd
  : ℕ
  → Bool

is-even zero
  = true
is-even (suc n)
  = is-odd n

is-odd zero
  = false
is-odd (suc n)
  = is-even n

is-even'
  : ℕ
  → Bool
is-even'
  = is-even

