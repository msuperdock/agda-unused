module SuppressDefinition where

open import Agda.Builtin.Char using (Char)
open import Agda.Builtin.Nat using (Nat)

infixr 5 _·_
infix  6 _^_

data Regex : Set where
  char  : Char → Regex
  digit : Regex
  _·_   : Regex → Regex → Regex
  _^_   : Regex → Nat → Regex
  -- ...

private
  iso8601-date : Regex -- agda-unused: ignore
  iso8601-date = (digit ^ 4) · char '-' · (digit ^ 2) · char '-' · (digit ^ 2)

  unused-regex : Regex
  unused-regex = digit
