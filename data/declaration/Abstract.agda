module Abstract where

abstract

  f
    : {A : Set}
    → A
    → A
  f x
    = x

  g
    : {A : Set}
    → A
    → A
  g x
    = x

  h
    : {A : Set}
    → A
    → A
  h x
    = f x

  _
    : {A : Set}
    → A
    → A
  _
    = λ x → x

