module Eval where

import           Control.Monad.Except
import           Env
import           Error
import           Native
import           Types

apply :: String -> [LispVal] -> LispVal
apply func args =
  case func of
    ()


eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val =
  case val of
    String _ ->
      return val

    Bool _ ->
      return val

    Number _ ->
      return val

    Symbol s ->
      getVar env s

    List [Symbol "quote", val] ->
      return val

    List [Symbol "define", Symbol var, form] ->
      eval env form >>= defineVar env var

    List [Symbol "if", pred, conseq, alt] -> do
      result <- eval env pred
      case result of
        Bool False -> eval env alt
        otherwise  -> eval env conseq

    List (Symbol func : args) -> do
      evaluatedArgs <-  traverse (eval env) args
      return $ apply func evaluatedArgs

    badForm ->
      throwError (BadSpecialForm badForm)
