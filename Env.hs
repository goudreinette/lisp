module Env where

import           Data.IORef
import           Data.Maybe
import           Error
import           Val
import Control.Monad.Trans.Except


type Env = IORef [(String, LispVal)]
type IOThrowsError = ExceptT LispError IO


newEnv :: IO Env
newEnv = newIORef []

-- getVar :: Env -> String ->
-- getVar envRef var =
