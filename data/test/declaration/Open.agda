module Open where

module M where

  data ⊤
    : Set
    where

    tt
      : ⊤

module N where

  open M

  x
    : ⊤
  x
    = tt

open M
open N

module O where

  y
    : ⊤
  y
    = tt

open O renaming
  ( y
    to z
  )

