module Parse (readOne, readMany, expr, parse) where

import           Control.Exception
import           Control.Monad.Except
import           Text.ParserCombinators.Parsec hiding (spaces, string)
import           Types


{- Symbols -}
symbolChar = oneOf "!#$%&|*+-/:<=>?@^_."

symbolStr :: Parser String
symbolStr = do
  first <- letter <|> symbolChar
  rest  <- many (letter <|> digit <|> symbolChar)
  return $ first:rest

symbol = do
  sym <- symbolStr
  return $ case sym of
    "true"  -> Bool True
    "false" -> Bool False
    "nil"   -> Nil
    _       -> Symbol sym


{- Numbers -}
number =
  (Number . read) <$> many1 digit


{- Strings -}
interpolation = do
  char '~'
  list <|> symbol

literalString =
  String <$> many1 (noneOf "\"~")

string = do
  char '"'
  xs <- many (literalString <|> interpolation)
  char '"'
  return $ List (Symbol "string-append" : xs)


{- Lists -}
list = do
  char '('
  contents <- sepBy expr spaces
  char ')'
  return $ List contents


{- Quoting -}
quote = do
  char '\''
  form <- expr
  return $ List [Symbol "quote", form]

unquote = do
  char '~'
  form <- expr
  return $ List [Symbol "unquote", form]


{- Whitespace -}
spaces = skipMany1 space

comment :: Parser ()
comment = do
  char ';'
  skipMany (noneOf "\n")


{- Lambda Shorthands -}
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


{- Expression -}
expr :: Parser LispVal
expr = lambda <|> symbol <|> number <|> string <|> list <|> quote <|> unquote


{- Parsing -}
exprSurroundedByWhitespace = do
  skipMany space
  e <- expr
  skipMany space
  return e

readOne :: String -> IO LispVal
readOne = parseSyntaxError exprSurroundedByWhitespace

readMany :: String -> IO [LispVal]
readMany = parseSyntaxError (many exprSurroundedByWhitespace)

parseSyntaxError :: Parser a -> String -> IO a
parseSyntaxError parser code =
  either (throw . SyntaxError)
         return
         (parse parser "lisp" code)
