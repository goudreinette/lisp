module Lisp where

import           Env
import           Error
import           Eval
import           Parse
import           Val



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
