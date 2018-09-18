-- https://tromp.github.io/cl/LC.pdf

import Calculus
import Parse

import System.IO

main = do
  putStrLn "Enter some lambda calculus:"
  repl

repl :: IO ()
repl = do
  putStr "λ>"
  hFlush stdout
  s <- getLine
  reduce $ parseBLC s
  repl

reduce :: Expression -> IO Expression
reduce x = if x /= beta x
  then do
    putStrLn "then"
    putStrLn $ show $ beta x
    reduce $ beta x
  else do
    putStrLn "else"
    putStrLn $ show $ beta x
    return x
