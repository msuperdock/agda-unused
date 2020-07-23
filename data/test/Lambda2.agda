module Lambda2 where

f
  : {A : Set}
  → A
  → A
f {A = A} x
  = (λ (y : A) (z : A) → z) x x

