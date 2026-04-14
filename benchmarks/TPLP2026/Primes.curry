{-# OPTIONS_FRONTEND -Wno-missing-signatures #-}

module Primes where

all_primes n = sieve (range 2 n)

sieve [] = []
sieve (n : ns) = (:) n $! (sieve $! remove_mults n ns)

remove_mults _ [] = []
remove_mults n (y : ys) =
  if mod y n > 0 then (:) y $! remove_mults n ys else remove_mults n ys

range m n = if m <= n then m : range (m + 1) n else []

mylength [] = 0
mylength (_ : xs) = mylength xs + 1

main = mylength $! all_primes 50000
