module Env where

import           Control.Monad.Except
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Data.IORef
import           Data.Maybe
import           Error
import           Val


type Env = IORef [(String, LispVal)]


newEnv :: IO Env
newEnv = newIORef []

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  case lookup var env of
    Nothing ->
      throwError $ UnboundVar var
    Just val ->
      return val
