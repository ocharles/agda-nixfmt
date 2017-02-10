module NixFmt where

open import Prelude
open import Container.Foldable using (foldMap)
open import WLPPrint

data Bindings : Set where

data Syntax : Set where
  string : List Char -> Syntax

parse : List Char → Maybe Syntax
parse ('"' ∷ '"' ∷ _) = just (string [])
parse _ = nothing

pretty-printer-for : (s : Syntax) → Set
pretty-printer-for s = Σ Doc (λ d → parse (display (render-pretty 80 d)) ≡ just s)

pretty : (l : Syntax) → pretty-printer-for l
pretty (string []) = char '\"' <> char '\"' , {!!}
pretty (string (x ∷ s)) = {!!}
