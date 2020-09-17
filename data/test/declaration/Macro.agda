module Macro where

module M where

  postulate

    A
      : Set

module N
  = M
  using (A)

module O
  = M
  renaming
  ( A
    to B
  )

C
  : Set
C
  = N.A

