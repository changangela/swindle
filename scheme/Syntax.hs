module Syntax where

-- an algebraic data type: it defines a set of possible values that a variable of type SchemeVal can hold
data SchemeVal
  = Atom String
  | List [SchemeVal]
  | DottedList [SchemeVal] SchemeVal
  | Number Integer
  | String String
  | Bool Bool
  deriving (Eq)

instance Show SchemeVal where
  show (Atom name) = name
  show (Number contents) = show contents
  show (String contents) = "\"" ++ contents ++ "\""
  show (Bool True) = "true"
  show (Bool False) = "false"
  show (List contents) =  if (null contents) then "'()"
                          else"(list " ++ unwordsList contents ++ ")"
  show (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"

unwordsList :: [SchemeVal] -> String
unwordsList = unwords . map show
