module Env where

import           Control.Lens               ((<&>))
import           Control.Monad.State.Strict
import           Data.IORef
import           Data.Maybe
import           Parse
import           Types

newEnv :: [(String, LispVal)] -> IO Env
newEnv vars = do
  e <- newIORef []
  evalStateT (bindVars e vars) []


trace val = do
  liftIO $ putStrLn $ showVal val
  return val

getReadtable :: Env -> CallstackIO [(ReadtableKey, String)]
getReadtable env = do
  List pairs <- getVar env "readtable"
  return $ map extractPair pairs
  where extractPair (List [String s, Symbol sym]) =
          (Prefix s, sym)
        extractPair (List [List [String start, String end], Symbol sym]) =
          (Between start end, sym)

isBound :: Env -> String -> CallstackIO Bool
isBound envRef var =
  liftIO $ readIORef envRef <&> lookup var <&> isJust

withVar :: Env -> String -> (IORef LispVal -> IO a) -> CallstackIO a
withVar envRef var f = do
  env <-  liftIO $ readIORef envRef
  case lookup var env of
    Just val -> liftIO $ f val
    _        -> throwWithStack $ UnboundVar var

getVar :: Env -> String -> CallstackIO LispVal
getVar envRef var =
  withVar envRef var readIORef

getVars :: Env -> CallstackIO [(String, LispVal)]
getVars envRef = do
  env <- liftIO $ readIORef envRef
  let vars = map fst env
  vals <- traverse (getVar envRef) vars
  return $ zip vars vals


setVar :: Env -> String -> LispVal -> CallstackIO LispVal
setVar envRef var value = do
  withVar envRef var (`writeIORef` value)
  return Nil



defineVar :: Env -> String -> LispVal -> CallstackIO LispVal
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

bindVars :: Env -> [(String, LispVal)] -> CallstackIO Env
bindVars envRef bindings = liftIO $ readIORef envRef >>= extendEnv >>= newIORef
     where extendEnv env = traverse addBinding bindings <&> (++ env)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)

