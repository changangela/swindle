module Syntax where
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
    -- Literal abstraction in order to differentiate between false and zero
    deriving (Eq, Show)

-- instance Show Expression where
--     show (Variable name) = name
--     show (Function identifier expression) = "(λ" ++ identifier ++ "." ++ show expression ++ ")"
--     show (Application expr1 expr2) = "(" ++ show expr1 ++ " " ++ show expr2 ++ ")"

data Literal
    = LInt Int
    | LBool Bool
    deriving (Show, Eq, Ord)

--abstractions = Map.fromList [

--	("Q", "λxy.y(S(xT))(xT)") -- increment a pair
--	("P", "λn.nQ(λx.xFF)F"),
--	(">", "λxy.Z(xPy)"),
--	(

-- zero λxy.y
_zero = Function "s" (Function "z" (Variable "z"))
--	succ λwyx.y(wyx)
_succ = Function "w" (Function "y" (Function "x" (Application (Variable "y") (Application (Application (Variable "w") (Variable "y")) (Variable "x")))))
--	add λxy.(x (succ(y)))
_add = Function "x" (Function "y" (Application (Variable "x") (Application _succ (Variable "y"))))
--	multiply λxyz.x(yz)
_multiply = Function "x" (Function "y" (Function "z" (Application (Variable "x") (Application (Variable "y") (Variable "z")))))
--	false λxy.y
_false = Function "x" (Function "y" (Variable "y"))
--	true λxy.x
_true = Function "x" (Function "y" (Variable "x"))
--	or λxy.xTy
_or = Function "x" (Function "y" (Application (Application (Variable "x") _true) (Variable "y")))
--	and λxy.xyF
_and = Function "x" (Function "y"(Application (Application (Variable "x") (Variable "y")) _false))
--	not λx.xFT
_not = Function "x" (Application (Application (Variable "x") _false) _true)
--	isZero λx.xF!F -- checks if a number is zero
_is_zero = Function "x" (Application (Application (Application (Variable "x") _false) _not) _false)

_number :: Int -> Expression
_number n = Function "s" (Function "z" (foldl (\expression temp -> (Application (Variable "s") expression)) (Variable "z") (take n (repeat 0))))

expressionsMap = Map.fromList [
    ("zero", _zero),
    ("succ", _succ),
    ("add", _add),
    ("multiply", _multiply),
    ("true", _true),
    ("false", _false),
    ("or", _or),
    ("and", _and),
    ("not", _not),
    ("is-zero", _is_zero)
    ]
