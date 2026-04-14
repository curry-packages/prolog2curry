{-# OPTIONS_FRONTEND -Wno-missing-signatures #-}

module Quicksort where

qsort [] = []
qsort (x : l) = app (qsort l1) (x : qsort l2)
  where
    (l1, l2) = partition x l

partition _ [] = ([],[])
partition y (x : l) =
  if x <= y
     then let (l1, l2) = partition y l in (x : l1,l2)
     else let (l1, l2) = partition y l in (l1,x : l2)

app [] xs = xs
app (x : xs) ys = x : app xs ys

main = qsort (app l6 l6)
  where
    l =
      [27
      ,74
      ,17
      ,33
      ,94
      ,18
      ,46
      ,83
      ,65
      ,2
      ,32
      ,53
      ,28
      ,85
      ,99
      ,47
      ,28
      ,82
      ,6
      ,11
      ,55
      ,29
      ,39
      ,81
      ,90
      ,37
      ,10
      ,0
      ,66
      ,51
      ,7
      ,21
      ,85
      ,27
      ,31
      ,63
      ,75
      ,4
      ,95
      ,99
      ,11
      ,28
      ,61
      ,74
      ,18
      ,92
      ,40
      ,53
      ,59
      ,8]
    l1 = app l l
    l2 = app l1 l1
    l3 = app l2 l2
    l4 = app l3 l3
    l5 = app l4 l4
    l6 = app l5 l5
