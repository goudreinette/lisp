module Native where

import           Env
import           Eval
import           Parse
import           System.Console.Repl
import           Types


primitives :: [(String, LispVal)]
primitives = purePrimitives ++ impurePrimitives


purePrimitives =
  wrapPrimitives Pure
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
  wrapPrimitives Impure
   [("read", readOne'),
    ("eval", eval'),
    ("env", env'),
    ("debug", debug)]


-- Wrap
wrapPrimitives c =
  map (fmap (PrimitiveFunc . c))

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
