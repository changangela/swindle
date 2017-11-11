module Eval where
import Data.Map (Map)
import qualified Data.Map as Map

-- a Name is a string used as an identifier and bound to an environment
type Name = [Char]





data Expression 
    -- Variable <Name> = <Name>
    = Variable Name
    -- Function <Variable> <Expression> = (λ<Variable>.<Expression>)
    | Function Name Expression
    -- Application <Expression> <Expression> = (<Expression> <Expression>)
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


-- λxy.y -> λx.(λy.(y))
false = Function "x" (Function "y" (Variable "y"))

expressionsMap = Map.fromList [("y", y), ("lambda", lambda), ("x", x), ("app", app), ("false", false)]

test :: String -> String
test name = do
    case (Map.lookup name expressionsMap) of
        Just expr -> show expr
        Nothing -> "error: '" ++ name ++ "' not found"
