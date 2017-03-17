module Repl where

import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Env
import           Eval
import           Parse
import           System.Console.ANSI
import           System.Console.Haskeline
import           System.IO
import           Types



repl = do
  hSetBuffering stdout NoBuffering
  globalEnv <- newEnv
  runInputT defaultSettings $ loop globalEnv

loop env = do
  line <- getInputLine "lisp=> "
  case line of
    Nothing -> return ()
    Just expr -> do
      evaled <- liftIO $ evalString env expr
      liftIO $ either printError print evaled
      loop env



evalString :: Env -> String -> IO (Either LispError LispVal)
evalString env expr =
  runExceptT (parseLine expr >>= eval env)

printError err = do
  setSGR [SetColor Foreground Vivid Red]
  print err
  setSGR [Reset]
