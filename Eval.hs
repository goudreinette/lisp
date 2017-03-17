module Eval where

import           Control.Monad.Except
import           Env
import           Native
import           Parse
import           Types

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args =
  return $ func args

apply (Func isMacro params body closure) args = do
  envWithArgs <- liftIO $ bindVars closure $ zip params args
  evalBody envWithArgs body


makeFn :: Bool -> [LispVal] -> [LispVal] -> Env -> IOThrowsError LispVal
makeFn isMacro params body env =
  return $ Func isMacro stringParams body env
  where stringParams = map extractString params
        extractString (Symbol s) = s

makeFunc = makeFn False
makeMacro = makeFn True


eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val =
  case val of
    String _ ->
      return val

    Bool _ ->
      return val

    Number _ ->
      return val

    List [] ->
      return val

    Symbol s ->
      getVar env s

    List [Symbol "env"] -> do
      vars <- getVars env
      return $ List $ map toPair vars
      where toPair (var, val) = List [Symbol var, val]

    List [Symbol "quote", form] ->
      evalUnquotes form
      where evalUnquotes form =
              case form of
                List [Symbol "unquote", form] ->
                  eval env form
                List items -> do
                  results <- mapM evalUnquotes items
                  return $ List results
                _ ->
                  return form

    List [Symbol "require", Symbol filepath] -> do
      contents <- liftIO $ readFile (filepath ++ ".lisp")
      form <- parseFile contents
      results <- mapM (eval env) form
      return $ List results

    List [Symbol "define", Symbol var, form] ->
      eval env form >>= defineVar env var

    List (Symbol "define" : List (Symbol var : params) : body) ->
      makeFunc params body env >>= defineVar env var

    List (Symbol "define-syntax" : List (Symbol var : params) : body) ->
      makeMacro params body env >>= defineVar env var

    List (Symbol "lambda" : List params : body) ->
      makeFunc params body env

    List [Symbol "if", pred, conseq, alt] -> do
      result <- eval env pred
      case result of
        Bool False -> eval env alt
        otherwise  -> eval env conseq

    List (func : args) -> do
      evaluatedFunc <- eval env func
      case evaluatedFunc of
        Func {isMacro = True} ->
          apply evaluatedFunc args >>= eval env

        _ -> do
          evaluatedArgs <- traverse (eval env) args
          apply evaluatedFunc evaluatedArgs

    badForm ->
      throwError (BadSpecialForm badForm)


evalMany env = traverse (eval env)

evalBody env body = last <$> evalMany env body

