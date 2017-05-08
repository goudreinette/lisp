module Main where

import           Control.Exception
import           Control.Monad       (join, when)
import           Data.Monoid         ((<>))
import           Env
import           Eval
import           Options.Applicative
import           Primitives
import           System.Console.Repl
import           Types





main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  <> header "Lisp"
  )
  where parser :: Parser (IO ())
        parser = work
            <$> optional (argument str (metavar "FILE"))
            <*> switch (long "interactive" <> short 'i' <> help "Launch a repl session")

work :: Maybe FilePath -> Bool -> IO ()
work file i = do
  globalEnv <- newEnv primitives
  maybe (return ()) (evalFile globalEnv) file
  when i $ interactive globalEnv
  where interactive env =
          repl "lisp=> " $ evalString env

