module Lisp where

import           Parse
import           Repl
import           Val
import           Env


main = repl eval parseLisp empty

eval = id
