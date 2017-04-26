module Parse (readOne, readMany, expr, parse) where

import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Reader
import           Text.ParserCombinators.Parsec hiding (spaces)
import           Types

type ReadTable = [(String, String)]





{- Whitespace -}
spaces = skipMany1 space

comment :: Parser ()
comment = do
  char ';'
  skipMany (noneOf "\n")


{- Expression -}
expr :: ReadTable -> Parser LispVal
expr readtable =
  e
  where e = p <|> lambda <|> symbol <|> number <|> string' <|> list
        p = readTableParser readtable

        {- Lambda Shorthands -}
        lambdaParam = do
          param <- char '%'
          return $ Symbol [param]

        lambda = do
          char '#'
          char '{'
          contents <- sepBy (e <|> lambdaParam) spaces
          char '}'
          let params = filter (== Symbol "%") contents
          return $ List [Symbol "lambda", List params, List contents]

        {- Lists -}
        list = do
          char '('
          contents <- sepBy e spaces
          char ')'
          return $ List contents

        {- Strings -}
        literalString =
          String <$> many1 (noneOf ('\"' : concatMap fst readtable))

        string' = do
          char '"'
          xs <- many (p <|> literalString)
          char '"'
          return $ List (Symbol "string-append" : xs)


        {- Symbols -}
        symbolChar = oneOf "!#$%&|*+-/:<=>?@^_."

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

{- Parsing -}
readTableParser :: ReadTable -> Parser LispVal
readTableParser readtable =
  foldl1 (<|>) $ map makeParser readtable
  where makeParser (s, sym) = do
          string s
          e <- expr readtable
          return $ List [Symbol sym, e]


exprSurroundedByWhitespace readtable = do
  skipMany space
  e <- expr  readtable
  skipMany space
  return e

readOne :: ReadTable -> String -> IO LispVal
readOne readtable = parseSyntaxError (exprSurroundedByWhitespace readtable)

readMany :: ReadTable -> String -> IO [LispVal]
readMany readtable = parseSyntaxError (many (exprSurroundedByWhitespace readtable))

parseSyntaxError :: Parser a -> String -> IO a
parseSyntaxError parser code =
  either (throw . SyntaxError)
         return
         (parse parser "lisp" code)
