module Env where

import           Control.Lens               ((<&>))
import           Control.Monad.State.Strict
import           Data.Either.Combinators
import           Data.IORef
import           Data.Maybe
import           Parse
import           Types

-- run :: LispM a -> IO a


newEnv :: [(String, LispVal)] -> IO Env
newEnv vars = do
  e <- newIORef []
  fromRight' <$> run (bindVars e vars)


trace val = do
  liftIO $ putStrLn $ showVal val
  return val

getReadtable :: Env -> LispM [(ReadtableKey, String)]
getReadtable env = do
  List pairs <- getVar env "readtable"
  return $ map extractPair pairs
  where extractPair (List [String s, Symbol sym]) =
          (Prefix s, sym)
        extractPair (List [List [String start, String end], Symbol sym]) =
          (Between start end, sym)

isBound :: Env -> String -> LispM Bool
isBound envRef var =
  liftIO $ readIORef envRef <&> lookup var <&> isJust

withVar :: Env -> String -> (IORef LispVal -> IO a) -> LispM a
withVar envRef var f = do
  env <-  liftIO $ readIORef envRef
  case lookup var env of
    Just val -> liftIO $ f val
    _        -> throwWithStack $ UnboundVar var

getVar :: Env -> String -> LispM LispVal
getVar envRef var =
  withVar envRef var readIORef

getVars :: Env -> LispM [(String, LispVal)]
getVars envRef = do
  env <- liftIO $ readIORef envRef
  let vars = map fst env
  vals <- traverse (getVar envRef) vars
  return $ zip vars vals


setVar :: Env -> String -> LispVal -> LispM LispVal
setVar envRef var value = do
  withVar envRef var (`writeIORef` value)
  return Nil



defineVar :: Env -> String -> LispVal -> LispM LispVal
defineVar envRef var value = do
  alreadyDefined <-  isBound envRef var
  if alreadyDefined
    then do
      setVar envRef var value
      return $ Symbol var
    else  do
      valueRef <- liftIO $ newIORef value
      env <-  liftIO $ readIORef envRef
      liftIO $ writeIORef envRef ((var, valueRef) : env)
      return $ Symbol var

bindVars :: Env -> [(String, LispVal)] -> LispM Env
bindVars envRef bindings = liftIO $ readIORef envRef >>= extendEnv >>= newIORef
     where extendEnv env = traverse addBinding bindings <&> (++ env)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)

