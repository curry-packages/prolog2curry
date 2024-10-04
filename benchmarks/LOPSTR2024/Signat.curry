{-# OPTIONS_FRONTEND -Wno-missing-signatures #-}

module Signat where

data Term = S Term | O
 deriving (Eq,Show)

signat O = O
signat (S x) = S O

plus O y = y
plus (S x) y = S (plus x y)

main | S O =:= signat (plus x O) = x
  where
    x free
