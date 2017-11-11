module Test where 
import Abstraction
import Data.Map (Map)
import qualified Data.Map as Map

-- some tests for Expressions
test :: String -> String
test name = do
    case (Map.lookup name expressionsMap) of
        Just expr -> show expr
        Nothing -> "error: '" ++ name ++ "' not found"