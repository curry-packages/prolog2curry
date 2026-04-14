{-# OPTIONS_FRONTEND -Wno-missing-signatures #-}

module Queens where

data Term = S Term | O
 deriving (Eq,Show)

queens n = selectqueens (range (S O) n) []

selectqueens [] qs = qs
selectqueens unplacedQs safeQs
  | noattack (S O) q safeQs
  = (selectqueens $! select q unplacedQs) (q : safeQs)
  where
    q free

noattack _ _ [] = True
noattack n x (y : ys)
  | neq x y && (neq x (plus y n) && (neq (plus x n) y && noattack (S n) x ys))
  = True

select x (x : xs) = xs
select x (y : ys) = (:) y $! select x ys

range m n = range_leq (leq m n) m n

range_leq True m n = m : range (S m) n
range_leq False _ _ = []

plus O n = n
plus (S x) y = S (plus x y)

neq O (S _) = True
neq (S _) O = True
neq (S x) (S y) | neq x y = True

leq O _ = True
leq (S _) O = False
leq (S x) (S y) = leq x y

queens4 = queens (S (S (S (S O))))

queens8 = queens (S (S (S (S (S (S (S (S O))))))))

main = queens8
