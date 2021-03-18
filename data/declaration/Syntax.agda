module Syntax where

data S
  (A₁ : Set)
  (A₂ : A₁ → Set)
  : Set
  where

  _,_
    : (x₁ : A₁)
    → A₂ x₁
    → S A₁ A₂

syntax S A₁ (λ x → A₂)
  = x ∈ A₁ × A₂

module M where

  data S'
    (A₁ : Set)
    (A₂ : A₁ → Set)
    : Set
    where

    _,'_
      : (x₁ : A₁)
      → A₂ x₁
      → S' A₁ A₂

  syntax S' A₁ (λ x → A₂)
    = x ∈' A₁ ×' A₂

open M
  using (S')

postulate

  p1
    : {A₁ : Set}
    → {A₂ : A₁ → Set}
    → x₁ ∈ A₁ × A₂ x₁
    → A₁
    
  p1'
    : {A₁ : Set}
    → {A₂ : A₁ → Set}
    → x₁ ∈' A₁ ×' A₂ x₁
    → A₁
  
