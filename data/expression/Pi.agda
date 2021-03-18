module Pi where

open import Agda.Builtin.Equality
  using (_≡_; refl)

f
  : {A : Set}
  → {x y : A}
  → (z w : A)
  → x ≡ z
  → z ≡ x
f _ _ refl
  = refl

