module Main where

import           Env
import           Eval
import           Primitives
import           System.Console.Repl
import           System.Environment



main = do
  env <- newEnv primitives
  args <- getArgs
  case args of
    [file, "-i"] -> do
      evalFile env file
      interactive env
    [file] ->
      evalFile env file
    [] ->
      interactive env
  where interactive env =
          repl "lisp=> " (evalString env)
