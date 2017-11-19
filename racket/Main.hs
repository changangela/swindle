module Main where
import Parser
import Eval
import Error
import System.IO
import Control.Monad
 
main :: IO ()

main = runRepl

-- REPL : read eval print loop
runRepl :: IO ()
runRepl = until_ (== "(exit)") (readPrompt "racket>> ") evalAndPrint

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action
