module Test where
import Data.Map (Map)
import qualified Data.Map as Map
import Abstraction

expressionsMap = Map.fromList [("y", y), ("lambda", lambda), ("x", x), ("app", app), ("false", false)]
test :: String -> String
test name = do
    case (Map.lookup name expressionsMap) of
        Just expr -> show expr
        Nothing -> "error: '" ++ name ++ "' not found"
