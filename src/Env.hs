module Env where

import           Control.Exception
import           Control.Lens      ((<&>))
import           Data.IORef
import           Data.Maybe
import           Types



newEnv :: [(String, LispVal)] -> IO Env
newEnv vars = do
  e <- newIORef []
  bindVars e vars


isBound :: Env -> String -> IO Bool
isBound env var =
  readIORef env <&> lookup var <&> isJust

withVar :: (IORef LispVal -> IO a) -> Env -> String -> IO a
withVar f env var= do
  env <-  readIORef env
  case lookup var env of
    Just val -> f val
    _        -> throw $ UnboundVar var

getVar :: Env -> String -> IO LispVal
getVar = withVar readIORef


setVar :: Env -> String -> LispVal -> IO LispVal
setVar env var value = do
  withVar (`writeIORef` value) env var
  return Nil


defineVar :: Env -> String -> LispVal -> IO LispVal
defineVar env var value = do
  valueRef <- newIORef value
  env' <-  readIORef env
  writeIORef env ((var, valueRef) : env')
  return $ Symbol var


bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars env bindings = readIORef env >>= extendEnv >>= newIORef
     where extendEnv env = traverse addBinding bindings <&> (++ env)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)
