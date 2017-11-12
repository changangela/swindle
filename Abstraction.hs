module Abstraction where
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

--abstractions = Map.fromList [
--	("*", "λxyz.x(yz)"),

--	("&&", "λxy.xy(F)"),
--	("||", "λxy.xTy"),
--	("!", "λx.xFT"),
--	("Z", "λx.xF!F"), -- checks if a number is zero
--	("Q", "λxy.y(S(xT))(xT)") -- increment a pair
--	("P", "λn.nQ(λx.xFF)F"),
--	(">", "λxy.Z(xPy)"),
--	(

--	succ λwyx.y(wyx)
_succ = Function "w" (Function "y" (Function "x" (Application (Variable "y") (Application (Application (Variable "w") (Variable "y")) (Variable "x")))))
--	add λxy.(x (succ(y)))
_add = Function "x" (Function "y" (Application (Variable "x") _succ ))
--	false λxy.y
_false = Function "x" (Function "y" (Variable "y"))
--	true λxy.x
_true = Function "x" (Function "y" (Variable "x"))
--	or λxy.xTy
_or = Function "x" (Function "y" (Application (Application (Variable "x") _true) (Variable "y")))




expressionsMap = Map.fromList [("succ", _succ), ("add", _add), ("true", _true), ("false", _false), ("or", _or)]
