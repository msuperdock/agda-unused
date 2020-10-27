module ModuleMacro where

record ⊤
  : Set
  where

module M where

module N where

  postulate

    A
      : Set

    B
      : Set

module O
  = M

module P
  = M

module Q
  = P

module R
  (x : ⊤)
  = N
  using (A)

module S
  = N
  renaming
  ( A
    to A'
  ; B
    to B'
  )

y
  : ⊤
y
  = record {O}

C
  : ⊤
  → Set
C
  = R.A

D
  : Set
D
  = S.B'

