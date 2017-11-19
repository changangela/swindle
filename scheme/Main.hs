module Main where
import Parser
import Eval
import Error
import System.IO
import Control.Monad
 
main :: IO ()

main = do
  expr <- prompt ">> "
  evaled <- return $ liftM show $ readExpr expr >>= eval
  putStrLn $ extractValue $ trapError evaled
  main

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine
