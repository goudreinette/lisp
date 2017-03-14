module Eval where

import           Env
import           Val

apply :: LispVal -> [LispVal] -> LispVal
apply (Symbol f) args =
  head args

eval :: LispVal -> Env -> LispVal
eval (List items) env =
  apply (eval (find env (head items)) env) (map (find env) (tail items))

eval primitive env =
  primitive
