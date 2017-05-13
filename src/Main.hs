module Main where

import           Env
import           Eval
import           Primitives
import           System.Console.Repl
import           System.Environment



main = do
  globalEnv <- newEnv primitives
  args <- getArgs
  case args of
    [file, "-i"] -> do
      evalFile globalEnv file
      interactive globalEnv
    [file] ->
      evalFile globalEnv file
    [] ->
      interactive globalEnv
  where interactive env =
          repl "lisp=> " (evalString env)


