module ModuleAlias where

-- The alias T collides with the data module T opened by this import; both
-- must stay reachable, so T.T and T.Apex resolve through the alias.
open import ModuleAliasDep
  as T
  using (T; Apex)

open T.T
  using (c; d)

f
  : Apex T
  → T
f a
  = psi
  where
    open T.Apex a

g
  : T
g
  = c
