{-# OPTIONS_FRONTEND -Wno-missing-signatures #-}

module Tak where

data Term = O | S Term
 deriving (Eq,Show)

add O n = n
add (S x) y = S (add x y)

double x = add x x

dec (S x) = x

leq O _ = True
leq (S _) O = False
leq (S x) (S y) = leq x y

tak x y z = takc (leq x y) x y z

takc True x y z = z
takc False x y z =
  ((tak $! (tak $! dec x) y z) $! (tak $! dec y) z x) $! (tak $! dec z) x y

two = S (S O)

four = double two

n8 = double four

n16 = double n8

n24 = add n8 n16

n27 = add (S two) n24

goal0 = tak n24 n16 n8

goal1 = tak n27 n16 n8

main = goal1
