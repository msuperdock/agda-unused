module Open2 where

data ⊤
  : Set
  where

  tt
    : ⊤

data ⊤'
  (x : ⊤)
  : Set
  where

  tt
    : ⊤' x

record R
  : Set
  where

  field

    x
      : ⊤

    y
      : ⊤

record S
  : Set₁
  where

  field

    x
      : R

  open R x public
    renaming (x to y; y to z)

postulate

  s
    : S

open S s
  using (y)

postulate

  p
    : ⊤' y

