module Env where

import           Control.Monad.Except
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Data.IORef
import           Data.Maybe
import           Native
import           Types


newEnv :: IO Env
newEnv = newIORef [] >>= \env -> bindVars env primitiveFuncs
  where primitiveFuncs = map (fmap PrimitiveFunc) primitives



isBound :: Env -> String -> IO Bool
isBound envRef var = do
  env <- readIORef envRef
  return $ isJust $ lookup var env


setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar var)
        (liftIO . (flip writeIORef value))
        (lookup var env)
  return value


getVars :: Env -> IOThrowsError [(String, LispVal)]
getVars envRef = do
  env <- liftIO $ readIORef envRef
  let vars = map fst env
  vals <- traverse (getVar envRef) vars
  return $ zip vars vals

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar var)
        (liftIO . readIORef)
        (lookup var env)

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then do
      setVar envRef var value
      return $ (Symbol var)
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return $ (Symbol var)

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)
