-- https://tromp.github.io/cl/LC.pdf

module Calculus where

import Debug.Trace
trace' = if True
  then \x y -> y -- K combinator
  else trace

data Variable = Variable String Int deriving Eq
inc :: Variable -> Variable
inc (Variable v n) = Variable v (n+1)

var_name :: Variable -> String
var_name (Variable s n) = s

instance Show Variable where
  show (Variable c n) = c ++ replicate n '\''

data Expression = Atom   Variable
                | Lambda Variable   Expression
                | Apply  Expression Expression
                deriving Eq

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

prelude ::[(String,Expression)]
prelude = 
  [("i", i)
  ,("true" , true)
  ,("false", false)
  ,("pair" , pair)
  ,("M"    , cM)
  ,("Y"    , cY)
  ]

beta :: Expression ->  Expression
beta (Atom a) = case lookup (var_name a) prelude of
                  Just e  -> e
                  Nothing -> Atom a 
beta t@(Lambda a b) = t
beta (Apply f@(Atom a) x) = Apply (beta f) (beta x)
beta (Apply (Lambda x m) n) = replace x n m
beta (Apply f x) = beta (Apply (beta f) (beta x))

-- Useful Terms from Tromp 2.1

i :: Expression
i = Lambda x (Atom x)
  where x = Variable "x" (-1)

true  = Lambda x (Lambda y $ Atom x)
  where
    x = Variable "x" (-1)
    y = Variable "y" (-1)
false = Lambda x (Lambda y $ Atom y)
  where
    x = Variable "x" (-1)
    y = Variable "y" (-1)

nil = false

-- We don't need arbitrary n-tuples
pair' p q = Lambda z $ Apply (Apply (Atom z) p) q
  where z = Variable "z" (-1)

pair :: Expression
pair = Lambda x $ Lambda y (pair' (Atom x) (Atom y))
  where
    x = Variable "x" (-1)
    y = Variable "y" (-1)

list [] = nil
list (x:xs) = pair' x (list xs)

-- M[n] list access
access m (-1) = Apply m true
access m n = access (Apply m false) (n-1)

-- M and Y combinators
-- no initial capitals because Haskell
cM = Lambda x $ Apply ox ox
  where
    ox = Atom x
    x = Variable "x" (-1)

cY = Lambda f $ Apply cM (Lambda x $ Apply (Atom f) (Apply (Atom x) (Atom x)))
  where
    x  = Variable "x" (-1)
    f  = Variable "f" (-1)

omega :: Expression
omega = Apply cM cM


