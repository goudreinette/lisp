module Lisp where

import           Env
import           Parse
import           Repl
import           Val


main = repl eval parseLisp

eval = id
