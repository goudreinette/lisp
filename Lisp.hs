module Lisp where

import           Env
import           Eval
import           Parse
import           Types



main = do
  globalEnv <- newEnv
  repl globalEnv

repl env = do
  putStr "lisp=> "
  line <- getLine
  result <- evalString env line
  case result of
    Left x  -> print x
    Right y -> print y
  repl env

evalString env expr =
  runIOThrows (parseLisp expr >>= eval env)
