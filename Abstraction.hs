module Abstraction where
import Data.Map (Map)
import qualified Data.Map as Map

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


false = Function "x" (Function "y" (Variable "y"))
expressionsMap = Map.fromList [("false", false)]
