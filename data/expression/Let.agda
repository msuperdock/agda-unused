module Let where

f
  : {A : Set}
  → A
  → A
f x
  = let
      y = x
      z = x
    in y

record Id
  (A : Set)
  : Set
  where

  constructor

    id

  field

    value
      : A

g
  : {A : Set}
  → Id A
  → A
g x'
  = let id y' = x'
    in let id z' = x'
    in y'

h
  : {A : Set}
  → A
  → A
h x''
  = let x'' = x'' in x''

i
  : {A : Set}
  → A
  → A
i x'''
  = let x'''@y'''@z''' = x''' in y'''

j
  : {A : Set}
  → A
  → A
j {A = A} x''''
  = let
    k : A → A → A
    k y'''' x'''' = y''''
    l : A → A
    l z'''' = z''''
  in k x'''' x''''

m
  : {A : Set}
  → Id A
  → A
m a
  = let module M = Id a in M.value

