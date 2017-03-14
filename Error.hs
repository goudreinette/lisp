module Error where

import           Control.Monad.Trans.Except
import           Text.ParserCombinators.Parsec (ParseError)

type IOThrowsError = ExceptT LispError IO

data LispError = UnboundVar String
               | SyntaxError ParseError

instance Show LispError where
  show (UnboundVar var) =
    "Unbound Var: " ++ var
  show (SyntaxError parseError) =
    "Syntax Error: " ++ show parseError
