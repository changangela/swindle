module Main where
import Parser
import Eval
import Syntax
import System.IO
import Control.Monad
import System.Console.Haskeline

main :: IO ()

main = runInputT defaultSettings runRepl

-- until_ :: InputT IO ()




-- REPL : read eval print loop
runRepl :: InputT IO ()
runRepl = do
  input <- getInputLine "swindle>> "
  case input of
    Nothing -> return ()
    Just "(exit)" -> return ()
    Just input -> liftIO $ (swindleEnv >>= (flip evalAndPrint input))
  runRepl


-- runRepl = swindleEnv >>= until_ (== "(exit)") (readPrompt "swindle>> ") . evalAndPrint

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn


flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

-- until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
-- until_ pred prompt action = do
--   result <- prompt
--   if pred result
--     then return ()
--     else action result >> until_ pred prompt action

-- runOne :: String -> IO ()
-- runOne expr = nullEnv >>= flip evalAndPrint expr

