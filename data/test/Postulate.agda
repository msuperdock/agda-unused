module Postulate where

postulate

  f
    : {A : Set}
    → A
    → A
  
  g
    : {A : Set}
    → A
    → A

h
  : {A : Set}
  → A
  → A
h x
  = f x

