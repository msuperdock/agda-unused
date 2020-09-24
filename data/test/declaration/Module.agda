module Module where

record R
  : Set
  where

module M where

module N where

module O where

  postulate

    A
      : Set

x
  : R
x
  = record {M}

module P
  = N

