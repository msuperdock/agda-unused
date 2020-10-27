module Test where

open import Agda.Builtin.Bool
  using (Bool; false; true)
open import Agda.Builtin.Unit

_∧_
  : Bool
  → Bool
  → Bool
false ∧ x
  = false
_ ∧ y
  = y
