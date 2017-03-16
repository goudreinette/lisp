module Lisp where

import           Control.Monad.Trans.Except
import           Env
import           Eval
import           Parse
import           System.Console.ANSI
import           Types



main = do
  globalEnv <- newEnv
  repl globalEnv

repl env =
  putStr "lisp=> "
  >>  getLine
  >>= evalString env
  >>= either printError print
  >>  repl env

evalString :: Env -> String -> IO (Either LispError LispVal)
evalString env expr =
  runExceptT (parseLine expr >>= eval env)

printError err = do
  setSGR [SetColor Foreground Vivid Red]
  print err
  setSGR [Reset]
