module Parse (readOne, readMany) where

import           Control.Exception
import           Text.ParserCombinators.Parsec hiding (spaces)
import           Types


{- Whitespace -}
spaces = skipMany1 space

{- Lists -}
list = do
  char '('
  contents <- sepBy expr spaces
  char ')'
  return $ List contents


{- Strings -}
string' = do
  char '"'
  xs <- many (noneOf "\"")
  char '"'
  return $ String xs


{- Symbols -}
symbolChar = oneOf "!#$%&|*+-/:<=>?@^_."

symbol = do
  first <- letter <|> symbolChar
  rest  <- many (letter <|> digit <|> symbolChar)
  return $ case first:rest of
    "true"  -> Bool True
    "false" -> Bool False
    "nil"   -> Nil
    sym     -> Symbol sym

{- Numbers -}
number =
  (Number . read) <$> many1 digit



{- Expression -}
expr :: Parser LispVal
expr  = symbol <|> number <|> string' <|> list


exprSurroundedByWhitespace = do
  skipMany space
  e <- expr
  skipMany space
  return e


{- Parsing -}
readOne :: String -> IO LispVal
readOne = parseSyntaxError exprSurroundedByWhitespace

readMany :: String ->IO [LispVal]
readMany = parseSyntaxError $ many exprSurroundedByWhitespace

parseSyntaxError :: Parser a -> String -> IO a
parseSyntaxError parser code =
  case parse parser "lisp" code  of
    Left e  -> throw $ SyntaxError e
    Right v -> return v
