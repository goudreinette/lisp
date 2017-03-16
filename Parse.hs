module Parse where

import           Control.Monad.Except
import           Text.ParserCombinators.Parsec hiding (spaces, string)
import           Types


symbolChar = oneOf "!#$%&|*+-/:<=>?@^_"

symbol = do
  first <- letter <|> symbolChar
  rest  <- many (letter <|> digit <|> symbolChar)
  let symbol = first:rest
  return $ case symbol of
    "true"  -> Bool True
    "false" -> Bool False
    "nil"   -> Nil
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
  form <- expr
  return $ List [Symbol "quote", form]

unquote = do
  char '~'
  form <- expr
  return $ List [Symbol "unquote", form]

spaces = skipMany1 space


lambdaParam = do
  param <- char '%'
  return $ Symbol [param]

lambda = do
  char '#'
  char '{'
  contents <- sepBy (expr <|> lambdaParam) spaces
  char '}'
  let params = filter (== Symbol "%") contents
  return $ List [Symbol "lambda", List params, List contents]


expr :: Parser LispVal
expr = lambda <|> symbol <|> number <|> string <|> list <|> quote <|> unquote

exprSurroundedByWhitespace = do
  skipMany space
  e <- expr
  skipMany space
  return e

parseLine :: String -> IOThrowsError LispVal
parseLine = parseSyntaxError exprSurroundedByWhitespace

parseFile :: String -> IOThrowsError [LispVal]
parseFile = parseSyntaxError (many exprSurroundedByWhitespace)

parseSyntaxError :: Parser a -> String -> IOThrowsError a
parseSyntaxError parser code =
  either (throwError . SyntaxError)
         return
         (parse parser "lisp" code)
