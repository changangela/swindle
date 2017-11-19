module Syntax where

-- an algebraic data type: it defines a set of possible values that a variable of type RacketVal can hold
data RacketVal
  = Atom String
  | List [RacketVal]
  -- | DottedList [RacketVal] RacketVal
  | Number Integer
  | String String
  | Bool Bool
  | Cond RacketVal RacketVal
  deriving (Eq)

instance Show RacketVal where
  show (Atom name) = name
  show (Number contents) = show contents
  show (String contents) = "\"" ++ contents ++ "\""
  show (Bool True) = "true"
  show (Bool False) = "false"
  show (List contents) =  if (null contents) then "'()"
                          else"(list " ++ unwordsList contents ++ ")"
  -- show (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"
  show (Cond question answer) = "[" ++ show question ++ " " ++ show answer ++ "]"
unwordsList :: [RacketVal] -> String
unwordsList = unwords . map show
