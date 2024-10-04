{-# OPTIONS_FRONTEND -Wno-missing-signatures #-}

module Permsort where

data Term = S Term | O
 deriving (Eq,Show)

insert x [] = [x]
insert x (y : ys) = x : (y : ys)
insert x (y : ys) = y : insert x ys

perm [] = []
perm (x : xs) = insert x (perm xs)

leq O _ = True
leq (S x) (S y) | leq x y = True

sorted [] = True
sorted [_] = True
sorted (x : (y : ys)) | leq x y && sorted (y : ys) = True

psort xs | sorted ys = ys
  where
    ys = perm xs

descList O = []
descList (S n) = S n : descList n

main10 = psort (descList (S (S (S (S (S (S (S (S (S (S O)))))))))))

main11 = psort (descList (S (S (S (S (S (S (S (S (S (S (S O))))))))))))

main12 = psort (descList (S (S (S (S (S (S (S (S (S (S (S (S O)))))))))))))

main = main12
