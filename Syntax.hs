module Syntax (module Syntax, module Control.Monad.Except) where
import Control.Monad.Except
import Text.ParserCombinators.Parsec.Error
import Data.IORef

{----------------+
|     SYNTAX     |
+----------------}

-- an algebraic data type: it defines a set of possible values that a variable of type RacketVal can hold
data RacketVal
  = Atom String
  | List [RacketVal]
  -- | DottedList [RacketVal] RacketVal
  | Number Integer
  | String String
  | Bool Bool
  | Cond RacketVal RacketVal
  | PrimitiveFunc ([RacketVal] -> ThrowsError RacketVal)
  | Func {
      params :: [String],
      vararg :: (Maybe String),
      body :: [RacketVal],
      closure :: Env
    }

showVal :: RacketVal -> String
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Bool True) = "true"
showVal (Bool False) = "false"
showVal (List contents) =  if (null contents) then "'()"
                        else"(list " ++ unwordsList contents ++ ")"
-- show (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"
showVal (Cond question answer) = "[" ++ show question ++ " " ++ show answer ++ "]"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) = 
  "(lambda (" ++ unwords (map show args) ++
    (case varargs of
       Nothing -> ""
       Just arg -> " . " ++ arg) ++ ") ...)"

instance Show RacketVal where show = showVal
  -- show (Atom name) = name
  -- show (Number contents) = show contents
  -- show (String contents) = "\"" ++ contents ++ "\""
  -- show (Bool True) = "true"
  -- show (Bool False) = "false"
  -- show (List contents) =  if (null contents) then "'()"
  --                         else"(list " ++ unwordsList contents ++ ")"
  -- -- show (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"
  -- show (Cond question answer) = "[" ++ show question ++ " " ++ show answer ++ "]"
  -- show (PrimitiveFunc _) = "<primitive>"
  -- show (Func {params = args, vararg = varargs, body = body, closure = env}) = 
  --   "(lambda (" ++ unwords (map show args) ++
  --     (case varargs of
  --        Nothing -> ""
  --        Just arg -> " . " ++ arg) ++ ") ...)"

unwordsList :: [RacketVal] -> String
unwordsList = unwords . map show

{--------------+
|     ERROR    |
+--------------}

data RacketError 
  = NumArgs Integer [RacketVal]
  | TypeMismatch String RacketVal
  | Parser ParseError
  | BadSpecialForm String RacketVal
  | NotFunction String String
  | UnboundVar String String
  | ConstantVar String String
  | Default String

instance Show RacketError where
  show (NumArgs expected found) = "expected " ++ show expected ++ " args, found values " ++ unwordsList found
  show (TypeMismatch expected found) = "invalid type: expected " ++ expected ++ ", found " ++ show found
  show (Parser parseErr) = "parse error at " ++ show parseErr
  show (BadSpecialForm message form) = show form ++ ": " ++ message
  show (NotFunction message func) = show func ++ ": " ++ message
  show (UnboundVar message varName) = varName ++ ": " ++ message
  show (ConstantVar message varName) = varName ++ ": " ++ message
  show (Default message) = message

-- instance Error RacketError where
  -- noMsg = Default "an error has occured"
  -- strMsg = Default

type ThrowsError = Either RacketError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

{--------------------+
|     ENVIRONMENT    |
+--------------------}

type Env = IORef [(String, IORef RacketVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ExceptT RacketError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError RacketVal
getVar envRef var = do 
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "getting an unbound variable" var) (liftIO . readIORef) (lookup var env)

setVar :: Env -> String -> RacketVal -> IOThrowsError RacketVal
setVar envRef var value = do 
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "setting an unbound variable" var) (liftIO . (flip writeIORef value)) (lookup var env)
  return value

defineVar :: Env -> String -> RacketVal -> IOThrowsError RacketVal
defineVar envRef var value = do
  defined <- liftIO $ isBound envRef var
  if defined
    then throwError $ ConstantVar "this name was defined previously and cannot be re-defined" var
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

bindVars :: Env -> [(String, RacketVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do 
            ref <- newIORef value
            return (var, ref)
