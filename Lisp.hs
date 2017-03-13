module Lisp where

import           Parse
import           Repl


main = repl eval parseLisp

eval = id
