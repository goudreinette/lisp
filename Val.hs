module Val where



data LispVal = Symbol String
             | List [LispVal]
             | Number Integer
             | String String
             | Bool Bool
             | Nil
             deriving (Show)


showListContents contents =
  unwords $ map showVal contents

showVal (Symbol s)   = s
showVal (List list)  = "(" ++ showListContents list  ++ ")"
showVal (Number n)   = show n
showVal (String s)   = "\"" ++ s ++ "\""
showVal (Bool True)  = "true"
showVal (Bool False) = "false"
showVal Nil          = "nil"

-- instance Show LispVal where
--   show = showVal
