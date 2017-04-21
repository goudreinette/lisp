module Main where

import           Env
import           Eval
import           System.Console.Repl

main = do
  globalEnv <- newEnv
  repl "lisp=> " $ evalString globalEnv
