module Types where

import           Control.Exception
import           Control.Monad.State.Strict
import           Data.IORef
import           Data.Typeable
import           Text.ParserCombinators.Parsec (ParseError)


{- Callstack -}
data Callframe = Callframe Fn Arguments
type Callstack = [Callframe]
type CallstackIO a = StateT Callstack IO a


instance Show Callframe where
  show (Callframe FnRecord {name = name} args) =
    "(" ++ showName name ++ " " ++ showListContents args ++ ")"



{- Env -}
type Env = IORef [(String, IORef LispVal)]


{- Error -}
throwWithStack :: ErrorType -> CallstackIO a
throwWithStack e = do
  stack <- get
  liftIO $ throw $ LispError e stack

type Expected = LispVal
type Got = LispVal



data LispError = LispError ErrorType Callstack
  deriving (Typeable)

data ErrorType = UnboundVar String
               | SyntaxError ParseError
               | NumArgs Int Int
               | TypeMismatch Expected Got
               | Default String
               deriving (Typeable)


instance Exception LispError

instance Show ErrorType where
  show (UnboundVar var) =
    "Unbound Var: " ++ var
  show (SyntaxError parseError) =
    "Syntax Error: " ++ show parseError
  show (NumArgs expected vals) =
    "Wrong number of arguments: expected " ++ show expected ++ ", got " ++ show vals

instance Show LispError where
  show (LispError errType stack) =
    show errType ++ "\n" ++ unlines (map show stack)



{- Val -}
type Arguments = [LispVal]

data FnName = Anonymous
            | Named String

data Purity = Pure ([LispVal] -> LispVal)
            | Impure (Env -> [LispVal] -> CallstackIO LispVal)

data FnType
  = Primitive { purity :: Purity }
  | Lisp { params  :: [String],
           varargs :: Bool,
           body    :: [LispVal],
           closure :: Env }

data Fn = FnRecord { name    :: FnName,
                     isMacro :: Bool,
                     fnType  :: FnType }


data LispVal = Symbol String
             | List [LispVal]
             | Number Integer
             | String String
             | Bool Bool
             | Nil
             | Fn Fn

-- TODO: unpack

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
instance Show LispVal where
  show = showVal


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


showFn :: Fn -> String
showFn FnRecord { fnType = fnType, isMacro = isMacro } =
  case fnType of
    Primitive {} ->
      "<primitive " ++ (if isMacro then "macro" else "function") ++ ">"
    Lisp {params = params, varargs = varargs, body = body} ->
       "(lambda " ++ showParams params varargs ++ " " ++ showListContents body  ++ ")"

showListContents =
  unwords . map show

showParams params varargs
  | varargs && (length params == 1) =
    head params

  | varargs =
    "(" ++ unwords (init params) ++ " . " ++ last params ++ ")"

  | otherwise =
    "(" ++ unwords params ++ ")"

showName name =
  case name of
    Named s   -> s
    Anonymous -> "<anonymous>"

