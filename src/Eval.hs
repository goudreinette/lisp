module Eval where

import           Control.Exception
import           Env
import           Parse
import           System.Console.Repl
import           Types


{- Eval -}
eval :: Env -> LispVal -> IO LispVal
eval env val =
  case val of
    Symbol s ->
      getVar env s

    List [Symbol "quote", form] ->
      evalUnquotes form
      where evalUnquotes val =
              case val of
                List [Symbol "unquote", val] ->
                  eval env val
                List xs ->
                  List <$> traverse evalUnquotes xs
                _ ->
                  return val

    List (Symbol "define" : args) ->
      case args of
        [Symbol var, form] ->
          eval env form >>= defineVar env var
        List (Symbol var : params) : body ->
          defineVar env var $ makeFn params body env

    List (Symbol "lambda" : List params : body) ->
      return $ makeFn params body env

    List (Symbol "if" : test : conseq : alt : _) -> do
      r <- eval env test
      eval env (if r /= Bool False then conseq else alt)

    List (fsym : args) -> do
      (Fn f) <- eval env fsym
      apply env f args >>= eval env

    _ ->
      return val


evalMany env = traverse (eval env)
evalBody env body = last <$> evalMany env body


evalString :: Env -> String -> IO ()
evalString env string =
  withCatch (readOne string >>= eval env >>= printVal)

evalFile :: Env -> String -> IO ()
evalFile env file =
  withCatch (readFile file >>= readMany >>= evalMany env >> return ())

withCatch x =
  catch x (printError :: LispError -> IO ())


{- Apply -}
apply :: Env -> FnType -> [LispVal] -> IO LispVal
apply env Primitive { purity = p } args =
  case p of
    Pure func ->
      return $ func args
    Impure func ->
      func env args

apply env (Lisp params varargs body closure) args =
  if length params /= length args && not varargs then
    throw $ NumArgs (length params) (length args)
  else do
    envWithArgs <- bindVars closure $ zipParamsArgs params varargs args
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



{- Fn -}
makeFn :: [LispVal] -> [LispVal] -> Env -> LispVal
makeFn params body env =
  Fn $ Lisp stringParams varargs body env
  where stringParams = filter (/= ".") $ map extractString params
        extractString (Symbol s) = s
        varargs = case drop (length params - 2) params of
          [Symbol ".", Symbol vararg] -> True
          _                           -> False


