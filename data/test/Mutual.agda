module Mutual where

data ℕ
  : Set
  where

  zero
    : ℕ

  suc
    : ℕ
    → ℕ

f
  : ℕ
  → ℕ

g
  : ℕ
  → ℕ

f zero
  = zero
f (suc n)
  = g n

g zero
  = zero
g (suc n)
  = f n

