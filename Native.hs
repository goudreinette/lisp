module Native where

import           Types




primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("=", equals),
              ("first", first),
              ("rest", rest),
              ("cons", cons),
              ("reverse", reverseList)]


numericBinop op params =
  Number $ foldl1 op $ map unpackNum params

equals (a:b:_) =
  Bool (a == b)

first (List (x:xs):_) =
  x

rest (List (x:xs):_) =
  List xs

cons (x:(List xs):_) =
  List (x:xs)

reverseList ((List xs):_) =
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
