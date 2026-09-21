{-# OPTIONS_FRONTEND -Wno-missing-signatures #-}

module Numnodes where

data Nat = O | S Nat
 deriving (Eq,Show)

data Tree a = Leaf a | Node (Tree a) (Tree a)
 deriving (Eq,Show)

numnodes (Leaf _) = S O
numnodes (Node m1 m2) = S (plus (numnodes m1) (numnodes m2))

plus O n = n
plus (S m) n = S (plus m n)

numnodes_7 | S (S (S (S (S (S (S O)))))) =:= numnodes t = t
  where
    t free

main = numnodes_7
