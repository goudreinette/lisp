module Error where

import           Control.Monad.Trans.Except
import           Text.ParserCombinators.Parsec (ParseError)


data LispError = UnboundVar String
               | SyntaxError ParseError


type IOThrowsError = ExceptT LispError IO
