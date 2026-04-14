{-# OPTIONS_FRONTEND -Wno-missing-signatures #-}

module Sublist where

app [] ys = ys
app (x : xs) ys = x : app xs ys

app3 xs ys zs = app (app xs ys) zs

sublist xs | xs =:= app3 _ sub _ = sub
  where
    sub free

main = sublist [1,2]
