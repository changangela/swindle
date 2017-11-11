module Parse where
import Eval
import System.IO

-- the interpreter controls the input and output

interpreter :: IO()

 -- this is what the console looks like
 -- the users input is also read in the meantime

interpreter = do
    expression <- prompt ">>> "
    putStrLn (parse expression)
    interpreter


-- the prompt function does a flush
prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine


-- the parsing function will turn the key words into lambda functions

parse :: String -> String
parse expression = test expression
