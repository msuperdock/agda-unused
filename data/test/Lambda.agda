module Lambda where

f
  : {A : Set}
  → A
  → A
f x
  = (λ y z → z) x x

