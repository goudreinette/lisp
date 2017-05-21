module Types where

import           Control.Exception
import           Data.IORef
import           Data.Typeable
import           Text.ParserCombinators.Parsec (ParseError)



{- Env -}
type Env = IORef [(String, IORef LispVal)]


{- Error -}
instance Exception LispError

data LispError = UnboundVar String
               | SyntaxError ParseError
               | NumArgs Int Int
               | TypeMismatch LispVal LispVal
               | Default String
               deriving (Typeable)


instance Show LispError where
  show (UnboundVar var) =
    "Unbound var: " ++ var
  show (SyntaxError parseError) =
    "Syntax error: " ++ show parseError
  show (NumArgs expected vals) =
    "Wrong number of arguments: expected " ++ show expected ++ ", got " ++ show vals




{- Val -}
type Arguments = [LispVal]

data Purity = Pure ([LispVal] -> LispVal)
            | Impure (Env -> [LispVal] -> IO LispVal)

data FnType
  = Primitive { purity :: Purity }
  | Lisp { params  :: [String],
           varargs :: Bool,
           body    :: [LispVal],
           closure :: Env }


data LispVal = Symbol String
             | List [LispVal]
             | Number Integer
             | String String
             | Bool Bool
             | Nil
             | Fn FnType
             deriving (Show)

-- TODO: unpack

instance Show FnType where
  show Primitive{} = "Primitive {..}"
  show Lisp{}      = "Lisp {..}"

instance Eq LispVal where
  Symbol a == Symbol b =
    a == b
  Number a == Number b =
    a == b
  List a == List b =
    a == b
  Bool a == Bool b =
    a == b
  Nil == Nil =
    True
  _ == _ =
    False



{- Show -}
printVal = putStrLn . showVal

showVal :: LispVal -> String
showVal val =
  case val of
  Symbol s   -> s
  List list  -> "(" ++ showListContents list  ++ ")"
  Number n   -> show n
  String s   -> "\"" ++ s ++ "\""
  Bool True  -> "true"
  Bool False -> "false"
  Nil        -> "nil"
  Fn f       -> showFn f


showFn :: FnType -> String
showFn f =
  case f of
    Primitive {} ->
      "<primitive>"
    Lisp {params = params, varargs = varargs, body = body} ->
       "(lambda " ++ showParams params varargs ++ " " ++ showListContents body  ++ ")"

showListContents =
  unwords . map showVal

showParams params varargs
  | varargs && (length params == 1) =
    head params

  | varargs =
    "(" ++ unwords (init params) ++ " . " ++ last params ++ ")"

  | otherwise =
    "(" ++ unwords params ++ ")"
