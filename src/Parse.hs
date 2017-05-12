module Parse (readOne, readMany) where

import           Control.Applicative.Alternative (asum)
import           Text.ParserCombinators.Parsec   hiding (spaces)
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
  where e = p <|> symbol <|> number <|> string' <|> list
        p = readTableParser readtable

        {- Lists -}
        list = do
          char '('
          contents <- sepBy e spaces
          char ')'
          return $ List contents

        {- Strings -}
        literalString =
          String <$> many1 (noneOf ('\"' : readtableKeys))
          where readtableKeys = concatMap fst readtable

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
  asum $ map makeParser readtable
  where makeParser (s, sym) = do
          string s
          e <- expr readtable
          return $ List [Symbol sym, e]


exprSurroundedByWhitespace readtable = do
  skipMany space
  e <- expr  readtable
  skipMany space
  return e

readOne :: ReadTable -> String -> CallstackIO LispVal
readOne readtable = parseSyntaxError (exprSurroundedByWhitespace readtable)

readMany :: ReadTable -> String -> CallstackIO [LispVal]
readMany readtable = parseSyntaxError (many (exprSurroundedByWhitespace readtable))

parseSyntaxError :: Parser a -> String -> CallstackIO a
parseSyntaxError parser code =
  either (throwWithStack . SyntaxError)
         return
         (parse parser "lisp" code)
