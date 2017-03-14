module Lisp where

import           Env
import           Error
import           Parse
import           Val

eval :: Env -> LispVal -> Either LispError LispVal
eval env val =
  Right val


main = do
  globalEnv <- newEnv
  repl globalEnv

repl globalEnv = do
  putStr "lisp=> "
  line <- getLine
  case parseLisp line >>= eval globalEnv of
    Left err ->
      print err
    Right result ->
      print result
  repl globalEnv
