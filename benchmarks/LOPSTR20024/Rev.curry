{-# OPTIONS_FRONTEND -Wno-missing-signatures #-}

module Rev where

data Term = O | S Term
 deriving (Eq,Show)

add O n = n
add (S x) y = S (add x y)

mult O _ = O
mult (S x) y = add y (mult x y)

two = S (S O)

four = add t t
  where
    t = two

nat16 = mult f f
  where
    f = four

nat256 = mult m m
  where
    m = nat16

nat4096 = mult nat256 nat16

app [] xs = xs
app (x : xs) ys = x : app xs ys

rev [] = []
rev (x : xs) = app (rev xs) [x]

natList O = []
natList (S x) = S x : natList x

isList [] = True
isList (_ : xs) = isList xs

main = isList (rev (natList nat4096))
