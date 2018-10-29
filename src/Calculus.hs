-- https://tromp.github.io/cl/LC.pdf

module Calculus where

import Debug.Trace
trace' = if True
  then \x y -> y -- K combinator
  else trace

data Variable = Variable String Int deriving Eq
inc :: Variable -> Variable
inc (Variable v n) = Variable v (n+1)

instance Show Variable where
  show (Variable c n) = c ++ replicate n '\''

data Expression = Atom   Variable
                | Lambda Variable   Expression
                | Apply  Expression Expression
                deriving Eq

-- see also  https://www.reddit.com/r/haskell/comments/9qx172/what_yous_favorite_weirdexotic_data_type/

{-
data Term v = Var v | App (Pair (Term v)) | Lam (Term (Incr v))
data Incr v = Zero | Succ v

which models the lambda calculus with correct-by-construction de bruijn indexing. This is the same approach taken by bound ( https://www.slideshare.net/ekmett/bound-making-de-bruijn-succ-less ).
-}

instance Show Expression where
  show (Atom v) = show v
  show (Lambda v l) = "(λ" ++ show v ++ "." ++ show l ++ ")"
  show (Apply  l m) = "("  ++ show l ++ " " ++ show m ++ ")"

-- replace  v     with    n      in    m      and   return
replace :: Variable -> Expression -> Expression -> Expression
replace v w (Atom u) = if v==u then w else Atom u
replace v n (Apply f a)     = Apply (replace v n f) (replace v n a)
replace v n w@(Lambda y x)    = 
  trace' ("replacing "++show v++" with "++show n++" in "++show w)
    (if v==y -- if there's a clash between v and a  bound variable
      then trace' ("clashing "++show v++" in "++show w++", replacing with "++show (inc v))
            replace v n (Lambda (inc v) $ replace y (Atom $ inc y) x)
           -- change the bound variable (alpha-conversion) and go again
      else Lambda y $ replace v n x)

beta :: Expression ->  Expression
beta (Atom a) = Atom a
beta t@(Lambda a b) = t
beta (Apply f@(Atom a) x) = Apply f (beta x)
beta (Apply (Lambda x m) n) = replace x n m
beta (Apply f x) = beta (Apply (beta f) (beta x))

{-
a :: Variable
a = Variable "a" 0
b = Variable "b" 0
c = Variable "c" 0
x = Variable "x" 0
y = Variable "y" 0
z = Variable "z" 0
-}

-- Useful Terms from Tromp 2.1

i :: Expression
i = Lambda x (Atom x)
  where x = Variable "x" 0

true  = Lambda x (Lambda y $ Atom x)
  where
    x = Variable "x" 0
    y = Variable "y" 0
false = Lambda x (Lambda y $ Atom y)
  where
    x = Variable "x" 0
    y = Variable "y" 0

nil = false

-- We don't need arbitrary n-tuples
pair p q = Lambda z $ Apply (Apply (Atom z) p) q
  where z = Variable "z" 0

list [] = nil
list (x:xs) = pair x (list xs)

-- M[n] list access
access m 0 = Apply m true
access m n = access (Apply m false) (n-1)

-- M and Y combinators
-- no initial capitals because Haskell
cM = Lambda x $ Apply ox ox
  where
    ox = Atom x
    x = Variable "x" 0

cY = Lambda f $ Apply cM (Lambda x $ Apply (Atom f) (Apply (Atom x) (Atom x)))
  where
    x  = Variable "x" 0 
    f  = Variable "f" 0

omega :: Expression
omega = Apply cM cM
