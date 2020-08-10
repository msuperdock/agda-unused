module Lam where

f
  : {A : Set}
  → A
  → A
f x
  = (λ y z → z) x x

g
  : {A : Set}
  → A
  → A
g {A = A} x
  = (λ (y' : A) (z : A) → z) x x

