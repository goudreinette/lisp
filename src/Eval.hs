module Eval where

import           Control.Exception
import           Control.Monad.State.Strict
import           Env
import           Parse
import           System.Console.Repl
import           Types

push :: Fn -> Arguments -> CallstackIO ()
push f args =
  modify addFrame
  where addFrame xs =
          Callframe f args : xs

pop :: CallstackIO ()
pop =
  modify popFrame
  where popFrame (_:xs) =
          xs
        popFrame xs =
          xs


{- Eval -}
eval :: Env -> LispVal -> CallstackIO LispVal
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
      (Fn f) <- eval env fsym
      push f args
      result <- if isMacro f then
                  apply env (fnType f) args >>= eval env
                else
                  evalMany env args >>= apply env (fnType f)
      pop
      return result

    _ ->
      return val


evalMany env = traverse (eval env)
evalBody env body = last <$> evalMany env body


evalString, evalFile :: Env -> String -> IO ()
evalString =
  evalWithCatch action
  where action env string = do
          readtable <- getReadtable env
          readOne readtable string >>= eval env >>= liftIO . putStrLn . showVal

evalWithInfo =
  evalWithCatch action
  where action env string = do
          readtable <- getReadtable env
          result <- readOne readtable string >>= eval env
          liftIO $ putStrLn $ showVal result ++ " : " ++ show result

evalFile =
  evalWithCatch action
  where action env file = do
          readtable <- getReadtable env
          liftIO (readFile file) >>= readMany readtable >>= evalMany env
          return ()


evalWithCatch f env x = do
  let action = evalStateT (f env x) []
  catch action (printError :: LispError -> IO ())


{- Apply -}
apply :: Env -> FnType -> [LispVal] -> CallstackIO LispVal
apply env Primitive { purity = p } args =
  case p of
    Pure func ->
      return $ func args
    Impure func ->
      func env args

apply env (Lisp params varargs body closure) args =
  if length params /= length args && not varargs then
    throwWithStack $ NumArgs (length params) (length args)
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
makeFn :: Bool -> FnName -> [LispVal] -> [LispVal] -> Env -> LispVal
makeFn isMacro name params body env =
  Fn $ FnRecord name isMacro $ Lisp stringParams varargs body env
  where stringParams = filter (/= ".") $ map extractString params
        extractString (Symbol s) = s
        varargs = case drop (length params - 2) params of
          [Symbol ".", Symbol vararg] -> True
          _                           -> False


