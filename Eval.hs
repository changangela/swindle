module Eval where
import Data.Map (Map)
import qualified Data.Map as Map
import Abstraction

-- Lambda includes name of arguments, expression, and its creation environment
data Lambda = Lambda {
    param :: Name,
    expression :: Expression,
    parent :: Environment
}

-- pretty printable
instance Show Lambda where
    show (Lambda param expression parent) = 
        "(λ" ++ param ++ "." ++ show expression ++ ")"


data Environment =
    Root
    | Environment Name Lambda Environment

environmentLookup :: Name -> Environment -> Lambda
environmentLookup item (Root) = error $ "error: couldn't find '" ++ item ++ "'"
environmentLookup item (Environment key value parent) = 
    if item == key then
        value
    else
        environmentLookup item parent


-- each type of AST node gets its own handler which returns a Lambda
evalExpression :: Environment -> Expression -> Lambda
evalExpression env (Function param body) = Lambda param body env
evalExpression env (Variable name) = environmentLookup name env
evalExpression env (Application function argument) = 
    let newArgument = evalExpression env argument
        newFunction = evalExpression env function
        newEnvironment = Environment (param newFunction) newArgument (parent newFunction)
        in evalExpression newEnvironment (expression newFunction)

eval :: Expression -> Lambda
eval expression = evalExpression Root expression

