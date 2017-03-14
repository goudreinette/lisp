module Eval where

import           Control.Monad.Except
import           Env
import           Native
import           Types

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args =
  func args

apply (Func params body closure) args = do
  envWithArgs <- liftIO $ bindVars closure $ zip params args
  evaluated <- evalBody envWithArgs
  return $ last evaluated
  where evalBody env = mapM (eval env) body


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
      evaluatedFunc <- eval env (Symbol func)
      evaluatedArgs <- traverse (eval env) args
      apply evaluatedFunc evaluatedArgs

    badForm ->
      throwError (BadSpecialForm badForm)
