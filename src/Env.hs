module Env where

import           Control.Exception
import           Control.Lens      ((&), (<&>))
import           Data.IORef
import           Data.Maybe
import           Types

newEnv :: [(String, LispVal)] -> IO Env
newEnv vars = do
  env <- newIORef []
  bindVars env vars

getReadtable :: Env -> IO [(String, String)]
getReadtable env = do
  List pairs <- getVar env "readtable"
  return $ map extractPair pairs
  where extractPair (List [Symbol s, Symbol sym]) =
          (s, sym)

isBound :: Env -> String -> IO Bool
isBound envRef var =
  readIORef envRef <&> lookup var <&> isJust

withVar :: Env -> String -> (IORef LispVal -> IO a) -> IO a
withVar envRef var f = do
  env <-  readIORef envRef
  case lookup var env of
    Just val -> f val
    _        -> throw $ UnboundVar var

getVar :: Env -> String -> IO LispVal
getVar envRef var =
  withVar envRef var readIORef

getVars :: Env -> IO [(String, LispVal)]
getVars envRef = do
  env <-  readIORef envRef
  let vars = map fst env
  vals <- traverse (getVar envRef) vars
  return $ zip vars vals


setVar :: Env -> String -> LispVal -> IO LispVal
setVar envRef var value = do
  withVar envRef var (`writeIORef` value)
  return value



defineVar :: Env -> String -> LispVal -> IO LispVal
defineVar envRef var value = do
  alreadyDefined <-  isBound envRef var
  if alreadyDefined
    then do
      setVar envRef var value
      return $ Symbol var
    else  do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return $ Symbol var

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv  >>= newIORef
     where extendEnv env = traverse addBinding bindings <&> (++ env)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)

