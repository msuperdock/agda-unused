module Do where

data Id
  (A : Set)
  : Set
  where

  id
    : A
    → Id A

_>>=_
  : {A B : Set}
  → Id A
  → (A → Id B)
  → Id B
id x >>= f
  = f x

f
  : {A : Set}
  → A
  → Id A
f x
  = do
    y <- id x
    z <- id x
    id y

