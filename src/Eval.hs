module Eval where

import           Control.Monad.Except
import           Env
import           Native
import           Parse
import           Types

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args =
  return $ func args

apply (Func isMacro params varargs body closure) args = do
  envWithArgs <- liftIO $ bindVars closure $ zipParamsArgs params varargs args
  evalBody envWithArgs body

zipParamsArgs :: [String] -> Bool -> [LispVal] -> [(String, LispVal)]
zipParamsArgs params varargs args =
  if varargs then
    let
      (normalargs, varargs) = splitAt (length params - 1) args
    in
      zip (init params) normalargs ++ [(last params, List varargs)]
  else
    zip params args


makeFn :: Bool -> [LispVal] -> [LispVal] -> Env -> IOThrowsError LispVal
makeFn isMacro params body env =
  return $ Func isMacro stringParams varargs body env
  where stringParams = filter (/= ".") $ map extractString params
        extractString (Symbol s) = s
        varargs = case drop (length params - 2) params of
          [Symbol ".", Symbol vararg] -> True
          _                           -> False

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
                  results <- traverse evalUnquotes items
                  return $ List results
                _ ->
                  return form

    List [Symbol "require", Symbol filepath] -> do
      contents <- liftIO $ readFile (filepath ++ ".lisp")
      forms <- parseFile contents
      results <- evalMany env forms
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
        _          -> eval env conseq

    List (func : args) -> do
      evaluatedFunc <- eval env func
      case evaluatedFunc of
        Func {isMacro = True} ->
          apply evaluatedFunc args >>= eval env

        _ ->
          evalMany env args >>= apply evaluatedFunc

    badForm ->
      throwError (BadSpecialForm badForm)


evalMany env = traverse (eval env)

evalBody env body = last <$> evalMany env body

