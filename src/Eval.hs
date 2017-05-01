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
      where evalUnquotes form =
              case form of
                List [Symbol "unquote", form] ->
                  eval env form
                List items ->
                  List <$> traverse evalUnquotes items
                _ ->
                  return form

    List (fsym : args) -> do
      f <- eval env fsym
      if isMacro f then
        apply env (fn f) args >>= eval env
      else
        evalMany env args >>= apply env (fn f)


    _ ->
      return val


evalMany env = traverse (eval env)
evalBody env body = last <$> evalMany env body


evalString, evalFile :: Env -> String -> IO ()
evalString env string = do
  readtable <- getReadtable env
  withCatch $
    readOne readtable string >>= eval env >>= print

evalFile env file = do
  readtable <- getReadtable env
  withCatch $ do
     readFile file >>= readMany readtable >>= evalMany env
     return ()

withCatch action =
  catch action (printError :: LispError -> IO ())




{- Apply -}
apply :: Env -> Fn -> [LispVal] -> IO LispVal
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
makeFn :: Bool -> [LispVal] -> [LispVal] -> Env -> IO LispVal
makeFn isMacro params body env =
  return $ Fn isMacro $ Lisp stringParams varargs body env
  where stringParams = filter (/= ".") $ map extractString params
        extractString (Symbol s) = s
        varargs = case drop (length params - 2) params of
          [Symbol ".", Symbol vararg] -> True
          _                           -> False

makeFunc = makeFn False
makeMacro = makeFn True
