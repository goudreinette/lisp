module Primitives where

import           Control.Lens               ((&), (<&>), (^.))
import qualified Control.Monad.State.Strict as State
import           Control.Monad.Trans
import           Data.List
import           Data.Maybe                 (fromJust)
import           Data.String.ToString
import           Env
import           Eval
import           Network.Wreq
import           Parse
import           System.Console.Repl
import           Text.URI
import           Types

primitives :: [(String, LispVal)]
primitives = purePrimitives ++ impurePrimitives ++ impurePrimitiveMacros ++ [("readtable", readtable)]

readtable =
  toLisp [("~", "unquote"),
          ("'", "quote"),
          ("^", "trace")]
  where toLisp = List . map toPair
        toPair (s, sym) = List [String s, Symbol sym]


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
    ("list", list),
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
     ("set!", set),
     ("lambda", lambda),
     ("if", if_),
     ("call/cc", callCC)]

-- Wrap
wrapPrimitives ismacro purity =
  map wrap
  where wrap (s, f) = (s, Fn $ FnRecord (Named s) ismacro $ Primitive $ purity f)

wrapPrimitive ismacro purity f = Fn $ FnRecord Anonymous ismacro $ Primitive $ purity f

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
  liftIO $ repl "debug=> " $ evalString env
  return Nil

print' _ [form] = do
  liftIO $ putStrLn $ showVal form
  return Nil

-- Impure Macro's
define isMacro env args =
  case args of
    [Symbol var, form] ->
      eval env form >>= defineVar env var
    List (Symbol var : params) : body ->
      makeFn isMacro (Named var) params body env & defineVar env var

set env [Symbol var, form] =
  eval env form >>= setVar env var


lambda env (List params : body) =
  return $ makeFn False Anonymous params body env

if_ env [pred, conseq, alt] = do
  result <- eval env pred
  return $ case result of
    Bool False -> alt
    _          -> conseq


callCC env [l] = do
  callback <- eval env l
  cont <- makeCont
  -- trace cont
  eval env (List [callback, cont])
  where makeCont = do
          contFnBody <- outerFrame >>= walk replaceContForm
          return $ makeFn False Anonymous [Symbol "x"] [List [Symbol "sc", contFnBody]] env

        extractCallframe (Callframe val) =
          val

        outerFrame = do
          stack <- State.get
          return $ fromJust $ find containsCallCCForm (map extractCallframe (reverse stack))
          where containsCallCCForm val =
                  case val of
                    List [Symbol "call/cc", _] -> True
                    List xs                    -> any containsCallCCForm xs
                    _                          -> False

        replaceContForm val =
          return $ case val of
            List [Symbol "call/cc", _] -> Symbol "x"
            _                          -> val

-- IO primitives
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
list xs =
  List xs

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
