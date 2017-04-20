module Repl where

import           Control.Monad.Trans.Except
import           Env
import           Eval
import           Parse
import           System.Console.ANSI
import qualified System.Console.Repl        as R
import           Types



repl = do
  globalEnv <- newEnv
  R.repl "lisp=> " $ evalString globalEnv



evalString :: Env -> String -> IO ()
evalString env expr = do
  result <- runExceptT (parseLine expr >>= eval env)
  either printError print result


printError err = do
  setSGR [SetColor Foreground Vivid Red]
  print err
  setSGR [Reset]
