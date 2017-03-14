module Types where

import           Control.Monad.Trans.Except
import           Data.IORef
import           Text.ParserCombinators.Parsec (ParseError)


-- Env
type Env = IORef [(String, IORef LispVal)]



-- Error
type IOThrowsError = ExceptT LispError IO
data LispError = UnboundVar String
               | SyntaxError ParseError
               | BadSpecialForm LispVal
               | NumArgs Integer [LispVal]
               | TypeMismatch LispVal
               | Default String

instance Show LispError where
  show (UnboundVar var) =
    "Unbound Var: " ++ var
  show (SyntaxError parseError) =
    "Syntax Error: " ++ show parseError
  show (BadSpecialForm specialForm) =
    "Unrecognized special form: " ++ show specialForm



-- Val
data LispVal = Symbol String
             | List [LispVal]
             | Number Integer
             | String String
             | Bool Bool
             | Nil
             | PrimitiveFunc ([LispVal] -> LispVal)
             | Func { params  :: [String],
                      body    :: [LispVal],
                      closure :: Env }

instance Show LispVal where
  show val =
    case val of
      Symbol s               -> s
      List list              -> "(" ++ showListContents list  ++ ")"
      Number n               -> show n
      String s               -> "\"" ++ s ++ "\""
      Bool True              -> "true"
      Bool False             -> "false"
      Nil                    -> "nil"
      PrimitiveFunc f        -> "<primitive function>"
      Func {params = params} -> "(lambda (" ++ unwords params ++ ") ...)"


showListContents contents =
  unwords $ map show contents