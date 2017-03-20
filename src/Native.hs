module Native where

import           Types




primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("=", boolBinop (==)),
              ("and", boolBinop (&&)),
              ("or", boolBinop (||)),
              ("first", first),
              ("rest", rest),
              ("cons", cons),
              ("reverse", reverseList)]

boolBinop op params =
  Bool $ foldl1 op $ map unpackBool params

numericBinop op params =
  Number $ foldl1 op $ map unpackNum params


first (List (x:xs):_) =
  x

rest (List (x:xs):_) =
  List xs


cons (x:List xs : _) =
  List (x:xs)

reverseList (List xs : _) =
  List (reverse xs)

--
-- numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> LispVal
-- numBoolBinop op params = Val.Bool $ foldl1 op $ map unpackNum params
--
-- boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> LispVal
-- boolBoolBinop op params = Val.Bool $ foldl1 op $ map unpackBool params


unpackNum :: LispVal -> Integer
unpackNum (Number n) = n

unpackBool:: LispVal -> Bool
unpackBool (Bool n) = n
