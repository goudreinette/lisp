module Env where

import           Control.Monad.Except
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Data.IORef
import           Data.Maybe
import           Native
import           Types

newEnv = newIORef []

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
        then setVar envRef var value >> return value
        else liftIO $ do
             valueRef <- newIORef value
             env <- readIORef envRef
             writeIORef envRef ((var, valueRef) : env)
             return value


bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)

-- getBindings :: Env -> IOThrowsError Bindings
-- getBindings envRef = do
--   envRecord <- liftIO $ readIORef envRef
--   return $ bindings envRecord

-- lookupVar :: Env -> IOThrowsError LispVal
