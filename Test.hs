module Test where
import Data.Map (Map)
import qualified Data.Map as Map
import Syntax

test :: String -> String
test name = do
    case (Map.lookup name expressionsMap) of
        Just expr -> show expr
        Nothing -> "error: '" ++ name ++ "' not found"
