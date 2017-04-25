module Native where

import           Parse
import           Types


primitives :: [(String, [LispVal] -> IO LispVal)]
primitives = [("+", numericBinop (+)),
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
              ("string-append", stringAppend),
              ("read", readOne')]

-- Read
readOne' [String s] =
  readOne s

-- Boolean
equals vals =
  return $ Bool $ all (== head vals) vals

-- Varargs
boolBinop op params =
   return $ Bool $ foldl1 op $ map unpackBool params

numericBinop op params =
  return $ Number $ foldl1 op $ map unpackNum params


-- List
first (List (x:xs):_) =
  return x

rest (List (x:xs):_) =
  return $ List xs

cons (x:List xs : _) =
  return $ List (x:xs)

reverseList (List xs : _) =
  return $ List (reverse xs)


-- String
stringAppend :: [LispVal] -> IO LispVal
stringAppend =
  return . String . concatMap stringVal


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
