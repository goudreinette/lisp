module Parse (parseLine, parseFile) where

import           Control.Monad.Except
import           Text.ParserCombinators.Parsec hiding (spaces, string)
import           Types


{- Symbols -}
symbolChar = oneOf "!#$%&|*+-/:<=>?@^_."

symbol = do
  first <- letter <|> symbolChar
  rest  <- many (letter <|> digit <|> symbolChar)
  let symbol = first:rest
  return $ case symbol of
    "true"  -> Bool True
    "false" -> Bool False
    "nil"   -> Nil
    _       -> Symbol symbol


{- Numbers -}
number =
  (Number . read) <$> many1 digit


{- Strings -}
string = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x


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

parseLine :: String -> IOThrowsError LispVal
parseLine = parseSyntaxError exprSurroundedByWhitespace

parseFile :: String -> IOThrowsError [LispVal]
parseFile = parseSyntaxError (many exprSurroundedByWhitespace)

parseSyntaxError :: Parser a -> String -> IOThrowsError a
parseSyntaxError parser code =
  either (throwError . SyntaxError)
         return
         (parse parser "lisp" code)
