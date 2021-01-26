module Open1 where

module M where

  data ⊤
    : Set
    where

    tt
      : ⊤

module N where

  open M

  v
    : ⊤
  v
    = tt

open M
open N

module O where

  w
    : ⊤
  w
    = tt

  x
    : ⊤
  x
    = tt

module P where

  open O
    using (w)
    renaming (x to x')

  y
    : ⊤
  y
    = w

open P

module Q where

  open O
    hiding (w)
    renaming (x to x'')

  z
    : ⊤
  z
    = x''

