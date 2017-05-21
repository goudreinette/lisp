module Primitives where

import           Control.Lens               ((&), (<&>), (^.))
import qualified Control.Monad.State.Strict as State
import           Control.Monad.Trans
import           Data.List
import           Data.Maybe                 (fromJust)
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
    ("car", first),
    ("cdr", rest),
    ("cons", cons),
    ("list", list),
    ("reverse", reverseList),
    ("string-append", stringAppend)]

impurePrimitives =
  wrapPrimitives Impure
   [("print", print'),
    ("slurp", slurp),
    ("spit", spit)]


{- Wrap -}
wrapPrimitives purity =
  map wrap
  where wrap (s, f) = (s, Fn $ Primitive $ purity f)



{- Impure Functions -}
print' _ [form] = do
  liftIO $ putStrLn $ showVal form
  return Nil

slurp _ [String s] =
  String <$> liftIO result
  where result =
          case parseURI s >>= uriScheme of
            Just _ -> get s <&> (^. responseBody) <&> toString
            _      -> readFile s


spit _ [String f, String s] = do
  liftIO $ writeFile f s
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
list =
  List

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
stringVal v          = showVal v
