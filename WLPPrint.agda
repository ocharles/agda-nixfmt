module WLPPrint where

open import Prelude hiding (if_then_else_ ; empty)

data Ungroup : Set where
  group-ok don't-group : Ungroup

data Doc : Set where
  empty : Doc
  char : Char → Doc
  text : List Char → Doc
  line : Ungroup → Doc
  cat union : Doc → Doc → Doc
  nest : Nat → Doc → Doc
  with-column with-nesting : (Nat → Doc) → Doc
  spaces : Nat → Doc

data SimpleDoc : Set where
  empty : SimpleDoc
  char : Char → SimpleDoc → SimpleDoc
  text : List Char → SimpleDoc → SimpleDoc
  line : Nat → SimpleDoc → SimpleDoc

instance
  MonoidDoc : Monoid Doc
  mempty {{MonoidDoc}} = empty
  _<>_ {{MonoidDoc}} = cat

indentation : Nat → List Char
indentation n = replicate n ' '

{-# NON_TERMINATING #-}
render-pretty : Nat → Doc → SimpleDoc
render-pretty w doc = best 0 0 [(0 , doc)] where

  nicest : Nat → Nat → SimpleDoc → SimpleDoc → SimpleDoc
  nicest n k x y = x

  best : Nat → Nat → List (Nat × Doc) → SimpleDoc
  best n k [] =
    empty

  best n k ((i , empty) ∷ xs) =
    best n k xs

  best n k ((i , char x) ∷ xs) =
    let k' = k + fromNat 1
    in char x (best n k' xs)

  best n k ((i , text x) ∷ xs) =
    let k' = k + length x
    in text x (best n k' xs)

  best n k ((i , line x) ∷ xs) =
    line i (best i i xs)

  best n k ((i , cat x y) ∷ xs) =
    best n k ((i , x) ∷ (i , y) ∷ xs)

  best n k ((i , union x y) ∷ xs) =
    nicest n k (best n k ((i , x) ∷ xs))
               (best n k ((i , y) ∷ xs))

  best n k ((i , nest j x) ∷ xs) =
    let i' = i + j
    in best n k ( (i' , x) ∷ xs)

  best n k ((i , with-column f) ∷ xs) =
    best n k ((i , f i) ∷ xs)

  best n k ((i , with-nesting f) ∷ xs) =
    best n k ((i , f i) ∷ xs)

  best n k ((i , spaces x) ∷ xs) =
    let k' = k + x
    in text (indentation x) (best n k' xs)

display : SimpleDoc → List Char
display empty = mempty
display (char x doc) = singleton x <> display doc
display (text x doc) = x <> display doc
display (line i x) = "\n" <> indentation i <> display x
