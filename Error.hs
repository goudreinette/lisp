module Error where

import           Control.Monad.Except
import           Control.Monad.Trans.Except
import           Text.ParserCombinators.Parsec (ParseError)
import           Val

type IOThrowsError = ExceptT LispError IO

data LispError = UnboundVar String
               | SyntaxError ParseError
               | BadSpecialForm LispVal

instance Show LispError where
  show (UnboundVar var) =
    "Unbound Var: " ++ var
  show (SyntaxError parseError) =
    "Syntax Error: " ++ show parseError
  show (BadSpecialForm specialForm) =
    "Unrecognized special form: " ++ show specialForm
