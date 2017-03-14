module Repl where

import           Val

repl show eval parse = do
  putStr "lisp=> "
  line <- getLine
  case parse line of
    Right val ->
      print $ eval val
    Left err ->
      print err
  repl show eval parse
