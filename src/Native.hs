module Native where

import           Eval
import           Parse
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
    ("eval", eval')]


-- Wrap
wrapPrimitives c =
  map (fmap (PrimitiveFunc . c))


-- Read
readOne' env [String s] =
  readOne s

-- Eval
eval' env (x:xs) =
  eval env x

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
