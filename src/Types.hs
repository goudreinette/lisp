module Types where

import           Control.Exception
import           Control.Monad.State.Strict
import           Data.IORef
import           Data.Typeable
import           Text.ParserCombinators.Parsec (ParseError)

-- Callstack
data Callframe = Callframe FnName Arguments
type Callstack = [Callframe]
type CallstackIO a = StateT Callstack IO a

-- Env
type Env = IORef [(String, IORef LispVal)]


-- Error
throwWithStack :: LispError -> CallstackIO a
throwWithStack e = do
  stack <- get
  liftIO $ throw $ LispErrorWithStack e stack

type Expected = LispVal
type Got = LispVal


data LispErrorWithStack =
  LispErrorWithStack LispError Callstack deriving (Typeable)

data LispError = UnboundVar String
               | SyntaxError ParseError
               | NumArgs Int Int
               | TypeMismatch Expected Got
               | Default String
              deriving (Typeable)


instance Exception LispError
instance Exception LispErrorWithStack

instance Show LispError where
  show (UnboundVar var) =
    "Unbound Var: " ++ var
  show (SyntaxError parseError) =
    "Syntax Error: " ++ show parseError
  show (NumArgs expected vals) =
    "Wrong number of arguments: expected " ++ show expected ++ ", got " ++ show vals

instance Show LispErrorWithStack where
  show (LispErrorWithStack e s) =
    show e ++ "\n" ++ unlines (map show s)



-- Val
type Arguments = [LispVal]

data FnName = Anonymous
            | Named String deriving (Show)

data Purity = Pure ([LispVal] -> LispVal)
            | Impure (Env -> [LispVal] -> CallstackIO LispVal)

data Fn = Primitive {purity :: Purity}
        | Lisp { name    :: FnName,
                 params  :: [String],
                 varargs :: Bool,
                 body    :: [LispVal],
                 closure :: Env }


instance Show Callframe where
  show (Callframe name args) =
    "(" ++ showName name ++ " " ++ showListContents args ++ ")"



data LispVal = Symbol String
             | List [LispVal]
             | Number Integer
             | String String
             | Bool Bool
             | Nil
             | Fn { isMacro :: Bool,
                    fn      :: Fn }

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
      Fn {isMacro = isMacro, fn = fn} ->
        case fn of
          Primitive purity ->
            "<primitive " ++ (if isMacro then "macro" else "function") ++ ">"

          Lisp {name = name, params = params, varargs = varargs, body = body} ->
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

