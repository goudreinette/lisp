module Lisp where

import           Control.Monad.Trans.Except
import           Env
import           Eval
import           Parse
import           Types



main = do
  globalEnv <- newEnv
  repl globalEnv

repl env =
  putStr "lisp=> "
  >>  getLine
  >>= evalString env
  >>= either print print
  >>  repl env

evalString :: Env -> String -> IO (Either LispError LispVal)
evalString env expr =
  runExceptT (parseLine expr >>= eval env)
