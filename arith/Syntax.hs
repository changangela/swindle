module Syntax where

data Expression
  = Tr
  | Fl
  | Zero
  | IsZero Expression
  | Succ Expression
  | Pred Expression
  | If Expression Expression Expression
  deriving Eq

instance Show Expression where
  show Zero = "0"
  show Tr = "true"
  show Fl = "false"
  show (Succ a) = "(succ " ++ show a ++ ")"
  show (Pred a) = "(pred " ++ show a ++ ")" 
  show (IsZero a) = "(iszero " ++ show a ++ ")"
  show (If a b c) = "if" ++ show a ++ "then" ++ show b ++ "else" ++ show c