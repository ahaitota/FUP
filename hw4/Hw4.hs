module Hw4 where

import Control.Applicative
import Data.Char
import Parser
import Hw3

-- Utility functions for parsing
token :: Parser a -> Parser a
token p = p <* spaces

spaces :: Parser ()
spaces = many (sat isSpace) *> pure ()

symbol :: String -> Parser String
symbol xs = token (string xs)

variable :: Parser String
variable = token (some (sat isAlphaNum))

-- Parsing expressions
expr :: Parser Expr
expr = app <|> lambda <|> var

var :: Parser Expr
var = Var <$> variable

lambda :: Parser Expr
lambda = do
    _ <- symbol "("
    _ <- symbol "\\"
    v <- variable
    _ <- symbol "."
    e <- expr
    _ <- symbol ")"
    return $ Lambda v e

app :: Parser Expr
app = do
    _ <- symbol "("
    e1 <- expr
    _ <- spaces
    e2 <- expr
    _ <- symbol ")"
    return $ App e1 e2

-- Parsing definitions
definition :: Parser (String, Expr)
definition = do
    v <- variable
    _ <- symbol ":="
    e <- expr
    return (v, e)

-- Parsing programs
program :: Parser ([(String, Expr)], Expr)
program = do
    defs <- many (definition <* spaces)
    mainExpr <- expr
    return (defs, mainExpr)

cnt :: Int -> String
cnt n = "a" ++ show n

check :: Symbol -> Expr -> Bool
check y (Var z) = y == z
check y (App z1 z2) = check y z1 || check y z2
check y (Lambda z e) = z /= y && check y e

sub :: Expr -> Symbol -> Expr -> Int -> Expr
sub (Var y) x z n = if y == x then z else Var y
sub (App e1 e2) x z n = App (sub e1 x z n) (sub e2 x z n)
sub (Lambda y e) x z n 
  | y == x = Lambda y e 
  | not (check y z) = Lambda y (sub e x z n) 
  | otherwise = Lambda (cnt n) (sub (sub e y (Var (cnt n)) (n+1)) x z (n+1)) 

-- Substitution function to resolve definitions
substituteDefs :: [(String, Expr)] -> Expr -> Expr
substituteDefs defs expr = foldr (\(var, val) acc -> sub acc var val 0) expr defs

-- Function to read and parse the program
readPrg :: String -> Maybe Expr
readPrg input = case parse program input of
    Nothing -> Nothing
    Just ((defs, mainExpr), _) -> Just (substituteDefs defs mainExpr)
