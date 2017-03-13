module Repl where

repl eval parse = do
  putStr "lisp=> "
  line <- getLine
  print $ eval $ parse line
  repl eval parse
