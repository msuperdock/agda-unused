module SuppressImport where

open import Agda.Builtin.Bool
  using (Bool; false; true) -- agda-unused: ignore
open import Agda.Builtin.Nat
  using (Nat; zero)
open import SuppressImportDep -- agda-unused: instances
  using (A)

g : Nat
g = zero

f : Bool
f = false
