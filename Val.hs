module Val where



data LispVal = Symbol String
             | List [LispVal]
             | Number Integer
             | String String
             | Bool Bool
             | Nil

instance Show LispVal where
  show val =
    case val of
      Symbol s   -> s
      List list  -> "(" ++ showListContents list  ++ ")"
      Number n   -> show n
      String s   -> "\"" ++ s ++ "\""
      Bool True  -> "true"
      Bool False -> "false"
      Nil        -> "nil"


showListContents contents =
  unwords $ map show contents
