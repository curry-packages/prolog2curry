{-# OPTIONS_FRONTEND -Wno-missing-signatures #-}

module Thirdof where

data Term = S Term | O
 deriving (Eq,Show)

plus O y = y
plus (S x) y = S (plus x y)

plus3 x y z = plus (plus x y) z

thirdOf x | x =:= plus3 y y y = y
  where
    y free

main = thirdOf O
