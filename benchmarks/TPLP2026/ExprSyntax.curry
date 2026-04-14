{-# OPTIONS_FRONTEND -Wno-missing-signatures #-}

module ExprSyntax where

data Term = Times | Leftbr | Rightbr | Zero | Plus | One
 deriving (Eq,Show)

exp = term
exp = app term (Plus : exp)

term = factor
term = app factor (Times : term)

factor = [Zero]
factor = [One]
factor = Leftbr : app exp [Rightbr]

app [] ys = ys
app (x : xs) ys = x : app xs ys

parse_exp | [Zero,Plus,One] =:= exp = True

gen_exp | (s =:= [_,_,_,_,_]) && (s =:= exp) = s
  where
    s free

main = gen_exp
