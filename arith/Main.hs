module Main where
import System.IO
import Eval
import Parser

main :: IO ()

-- this is what the console looks like
-- the users input is also read in the meantime

main = do
    input <- prompt "arith> "
    process input
    main

-- the prompt function does a flush

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

-- processes the input

process :: String -> IO()

process line = do
    let res = parseLn line
    case res of
        Left err -> print err
        Right ex -> case eval ex of
            Nothing -> putStrLn ("cannot evaluate '" ++ line ++ "'" )
            Just result -> putStrLn (show result)