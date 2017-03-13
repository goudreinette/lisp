module Parse where

import           Flow
import           System.Environment
import           Text.ParserCombinators.Parsec hiding (spaces, string)

data LispVal = Symbol String
             | List [LispVal]
             | Number Float
             | String String
             | Bool Bool
             deriving (Show)

symbolChar = oneOf "!#$%&|*+-/:<=>?@^_~"

symbol = do
  first <- letter <|> symbolChar
  rest  <- many (letter <|> digit <|> symbolChar)
  let symbol = first:rest
  return $ case symbol of
    "true"  -> Bool True
    "false" -> Bool False
    _       -> Symbol symbol

number =
  (Number . read) <$> many1 digit

string = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x

list = do
  char '('
  contents <- sepBy expr spaces
  char ')'
  return $ List contents

quote = do
  char '\''
  contents <- expr
  return $ List [Symbol "quote", contents]

spaces = skipMany1 space

expr :: Parser LispVal
expr = (symbol <|> number <|> string <|> list <|> quote)

exprSurroundedByWhitespace = do
  skipMany space
  e <- expr
  skipMany space
  return e

parseLisp = parse exprSurroundedByWhitespace "lisp"
