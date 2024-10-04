{-# OPTIONS_FRONTEND -Wno-missing-signatures #-}

module Numleaves where

data Nat = O | S Nat
 deriving (Eq,Show)

data Tree a = Leaf a | Node (Tree a) (Tree a)
 deriving (Eq,Show)

numleaves (Leaf _) = S O
numleaves (Node m1 m2) = S (plus (numleaves m1) (numleaves m2))

plus O n = n
plus (S m) n = S (plus m n)

main | S (S (S (S (S (S (S O)))))) =:= numleaves t = t
  where
    t free
