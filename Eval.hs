module Eval where
import qualified Data.Map as Map

-- a Name is a string used as an identifier and bound to an environment
type Name = [Char]

data Expression 
    = Variable Name
    | Function Name Expression
    | Application Expression Expression
    deriving Eq

instance Show Expression where
    show (Variable name) = name
    show (Function identifier expression) = "(λ" ++ identifier ++ "." ++ show expression ++ ")"
    show (Application expr1 expr2) = "(" ++ show expr1 ++ " " ++ show expr2 ++ ")"



-- some tests for Expressions

y = Variable "y"
lambda = Function "y" y
x = Variable "x"
app = Application lambda x

testExpressions = Map.fromList [("y", y), ("lambda", lambda), ("x", x), ("app", app)]

test :: String -> String
test name = show (Map.lookup name testExpressions)