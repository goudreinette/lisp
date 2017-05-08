module Primitives where

import           Control.Lens         ((<&>), (^.))
import           Control.Monad.Trans
import           Data.String.ToString
import           Env
import           Eval
import           Flow
import           Network.Wreq
import           Parse
import           System.Console.Repl
import           Text.URI
import           Types


primitives :: [(String, LispVal)]
primitives = purePrimitives ++ impurePrimitives ++ impurePrimitiveMacros ++ [("readtable", readtable)]

readtable =
  List [List [Symbol "~", Symbol "unquote"],
        List [Symbol "'", Symbol "quote"]]


purePrimitives =
  wrapPrimitives False Pure
   [("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("=", equals),
    ("and", boolBinop (&&)),
    ("or", boolBinop (||)),
    ("first", first),
    ("rest", rest),
    ("cons", cons),
    ("reverse", reverseList),
    ("string-append", stringAppend)]

impurePrimitives =
  wrapPrimitives False Impure
   [("read", read'),
    ("read-many", readMany'),
    ("eval", eval'),
    ("unquote", eval'),
    ("env", env'),
    ("debug", debug),
    ("print", print'),
    ("slurp", slurp),
    ("spit", spit)]

impurePrimitiveMacros =
  wrapPrimitives True Impure
    [("define", define False),
     ("define-syntax", define True),
     ("lambda", lambda),
     ("if", if_)]

-- Wrap
wrapPrimitives macro c =
  map $ fmap  $ Fn macro "<primitive>" . Primitive . c


-- Impure Functions
read' env [String s] = do
  readtable <- getReadtable env
  readOne readtable s

readMany' env [String s] = do
  readtable <- getReadtable env
  List <$> readMany readtable s

eval' env (x:_) =
  eval env x

env' env [] =
  fmap (List . map toPair) (getVars env)
  where toPair (var, val) = List [Symbol var, val]

debug env [] = do
  repl "debug=> " $ evalString env
  return Nil

print' _ [form] = do
  print form
  return Nil

-- Impure Macro's
define isMacro env args =
  case args of
    [Symbol var, form] ->
      eval env form >>= defineVar env var
    List (Symbol var : params) : body ->
      makeFn isMacro var params body env >>= defineVar env var

lambda env (List params : body) =
  makeFn False "<anonymous>" params body env

if_ env [pred, conseq, alt] = do
  result <- eval env pred
  case result of
    Bool False -> eval env alt
    _          -> eval env conseq

-- IO primitives
slurp _ [String s] =
  String <$> case parseURI s >>= uriScheme of
    Just _ -> get s <&> (^. responseBody) <&> toString
    _      -> readFile s

spit _ [String f, String s] = do
  writeFile f s
  return Nil


-- Boolean
equals vals =
   Bool $ all (== head vals) vals

-- Varargs
boolBinop op params =
  Bool $ foldl1 op $ map unpackBool params

numericBinop op params =
  Number $ foldl1 op $ map unpackNum params


-- List
first (List (x:xs):_) =
  x



rest (List (x:xs):_) =
  List xs

rest [List x] =
  List x

cons (x:List xs : _) =
  List (x:xs)

reverseList (List xs : _) =
  List (reverse xs)


-- String
stringAppend :: [LispVal] -> LispVal
stringAppend =
  String . concatMap stringVal


--
-- numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> LispVal
-- numBoolBinop op params = Val.Bool $ foldl1 op $ map unpackNum params
--
-- boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> LispVal
-- boolBoolBinop op params = Val.Bool $ foldl1 op $ map unpackBool params

-- Unpack

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n

unpackBool:: LispVal -> Bool
unpackBool (Bool b) = b



stringVal (String s) = s
stringVal v          = show v
