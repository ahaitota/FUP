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
    spaces
    return (defs, mainExpr)

-- Substitution function to resolve definitions
substituteDefs :: [(String, Expr)] -> Expr -> Expr
substituteDefs defs expr = foldl (\acc (var, val) -> substitute acc var val 0) expr defs

-- Function to read and parse the program
readPrg :: String -> Maybe Expr
readPrg input = case parse program input of
    Nothing -> Nothing
    Just ((defs, mainExpr), _) -> Just (substituteDefs defs mainExpr)
