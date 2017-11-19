module Main where
import System.IO
import System.Environment
import Parser
import Eval
 
main :: IO ()

main = do
  expr <- prompt ">>> "
  print (eval (readExpr expr))
  main

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine
