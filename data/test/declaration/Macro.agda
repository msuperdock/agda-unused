module Macro where

data ⊤
  : Set
  where

  tt
    : ⊤

module M where

  postulate

    A
      : Set

module N
  (x : ⊤)
  = M
  using (A)

module O
  = M
  renaming
  ( A
    to B
  )

C
  : ⊤
  → Set
C
  = N.A

