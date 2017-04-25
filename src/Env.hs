module Env where

import           Control.Exception
import           Control.Monad.Trans
import           Data.IORef
import           Data.Maybe
import           Data.Typeable
import           Types


newEnv :: [(String, LispVal)] -> IO Env
newEnv vars = newIORef [] >>= \env -> bindVars env vars


isBound :: Env -> String -> IO Bool
isBound envRef var = do
  env <- readIORef envRef
  return $ isJust $ lookup var env


setVar :: Env -> String -> LispVal -> IO LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throw $ UnboundVar var)
        (liftIO . flip writeIORef value)
        (lookup var env)
  return value


getVars :: Env -> IO [(String, LispVal)]
getVars envRef = do
  env <- liftIO $ readIORef envRef
  let vars = map fst env
  vals <- traverse (getVar envRef) vars
  return $ zip vars vals

getVar :: Env -> String -> IO LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throw $ UnboundVar var)
        (liftIO . readIORef)
        (lookup var env)

defineVar :: Env -> String -> LispVal -> IO LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then do
      setVar envRef var value
      return $ Symbol var
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return $ Symbol var

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv bindings env = fmap (++ env) (mapM addBinding bindings)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)
