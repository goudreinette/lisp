module Primitives where

import           Control.Monad.Trans
import           Env
import           Eval
import           Flow
import           Parse
import           System.Console.Repl
import           Types


primitives :: [(String, LispVal)]
primitives = purePrimitives ++ impurePrimitives ++ impurePrimitiveMacros


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
   [("read", readOne'),
    ("eval", eval'),
    ("env", env'),
    ("debug", debug)]

impurePrimitiveMacros =
  wrapPrimitives True Impure
    [("require", require),
     ("define", define),
     ("define-syntax", defineSyntax),
     ("lambda", lambda),
     ("if", if_),
     ("quote", quote)]

-- Wrap
wrapPrimitives macro c =
  map (fmap (PrimitiveFunc macro . c))


-- Impure
readOne' _ [String s] =
  readOne s

eval' env (x:_) =
  eval env x

env' env [] =
  fmap (List . map toPair) (getVars env)
  where toPair (var, val) = List [Symbol var, val]

debug env [] = do
  repl "debug=> " $ evalString env
  return Nil

-- Impure Macro's
require env [Symbol filepath] = do
  evalFile env (filepath ++ ".lisp")
  return Nil

define env [Symbol var, form] =
  eval env form >>= defineVar env var

define env (List (Symbol var : params) : body) =
  makeFunc params body env >>= defineVar env var

defineSyntax env (List (Symbol var : params) : body) =
  makeMacro params body env >>= defineVar env var

lambda env (List params : body) =
  makeFunc params body env

-- Pure macro's
if_ env [pred, conseq, alt] = do
  result <- eval env pred
  case result of
    Bool False -> eval env alt
    _          -> eval env conseq


quote env [form] = do
  result <- evalUnquotes form
  return $ List [Symbol "quote", result]
  where evalUnquotes form =
          case form of
            List [Symbol "unquote", form] ->
              eval env form
            List items -> do
              results <- traverse evalUnquotes items
              return $ List results
            _ ->
              return form

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


unpackString (String s) = s

stringVal (String s) = s
stringVal v          = show v
