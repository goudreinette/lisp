module Main where

import           Env
import           Eval
import qualified Native
import           System.Console.Repl
import           System.Environment  (getArgs)

primitives = Native.primitives

main = do
  args <- getArgs
  globalEnv <- newEnv primitives
  case args of
    [file] ->
      evalFile globalEnv file
    [file, "-i"] -> do
      evalFile globalEnv file
      interactive globalEnv
    [] ->
      interactive globalEnv
  where interactive env =
          repl "lisp=> " $ evalString env
