module Eval where
import Data.Map (Map)
import qualified Data.Map as Map
import Syntax
import Debug.Trace

-- Lambda includes name of arguments, expression, and its creation environment
data Lambda = Lambda {
    param :: Name,
    expression :: Expression,
    parent :: Content
}

-- pretty printable
instance Show Lambda where
    show (Lambda param expression parent) = 
        "(λ" ++ param ++ "." ++ show expression ++ ")"


data Content =
    Root
    | Content Name Lambda Content

environmentLookup :: Name -> Content -> Lambda
environmentLookup item (Root) = Lambda item (Variable item) Root
environmentLookup item (Content key value parent) = 
    if item == key then
        value
    else
        environmentLookup item parent


-- each type of AST node gets its own handler which returns a Lambda
evalExpression :: Content -> Expression -> Lambda


evalExpression env (Function param body) = Lambda param body env

-- evalExpression env (Function param body) = Lambda param body env

-- evalExpression env (Variable name) = trace (show (environmentLookup name env)) (environmentLookup name env)
evalExpression env (Variable name) = environmentLookup name env

evalExpression env (Application function argument) = 
    let newArgument = evalExpression env argument
        newFunction = evalExpression env function
        newEnvironment = Content (param newFunction) newArgument (parent newFunction)
        in evalExpression newEnvironment (expression newFunction)

-- eval :: Expression -> Lambda
-- eval expression = evalExpression Root expression

-- change implementation of Environment map and no longer lazy val

type Environment = [(String, Expression)]

eval :: Environment -> Expression -> Expression
eval env (Variable name) = maybe (Variable name) id (lookup name env)
eval env (Function name expression) = Function name (eval env expression)
eval env (Application function argument) = (apply env (eval env function) (eval env argument))

apply :: Environment -> Expression -> Expression -> Expression
apply env (Function name expression) argument = eval ((name, argument): env) expression
apply env e1 e2 = Application e1 e2

