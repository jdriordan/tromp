{-# LANGUAGE OverloadedStrings #-}
module Parse where

import Calculus

import Text.ParserCombinators.Parsec
import System.Environment
import Control.Monad

lambda :: Parser Char
lambda = oneOf ['λ', '\\']

parseVariable :: Parser Variable
parseVariable = do
    name   <- many1 letter 
    primes <- many $ char '\''
    return $ Variable name (length primes)

parseExpression :: Parser Expression
parseExpression =
  (liftM Atom) parseVariable <|>
  do
    char '('
    x <- try parseLambda <|> parseApply
    char ')'
    return x

parseLambda :: Parser Expression
parseLambda = do
  lambda >> spaces
  v <- parseVariable
  char '.' >> spaces
  e <- parseExpression
  return $ Lambda v e

parseApply :: Parser Expression
parseApply = do
  f <- parseExpression
  spaces
  x <- parseExpression
  return $ Apply f x

parseBLC :: String -> Expression
parseBLC s = get $ parse parseExpression "" s
  where
    get (Left  err) = Atom $ Variable ("error-"++show err) 0
    get (Right exp) = exp
