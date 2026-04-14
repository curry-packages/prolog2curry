{-# OPTIONS_FRONTEND -Wno-missing-signatures #-}

module Pali where

app [] ys = ys
app (x : xs) ys = x : app xs ys

app3 xs ys zs = app (app xs ys) zs

rev [] = []
rev (x : xs) = app (rev xs) [x]

pali zs | zs =:= app3 xs [x] (rev xs) = x
  where
    xs, x free

main = pali [1,2,3,2,1]
