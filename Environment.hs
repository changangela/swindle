module Environment where
import Data.IORef
import Syntax
import Error

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

-- initialize some keywords
swindleEnv :: IO Env
swindleEnv = nullEnv >>= (flip bindVars [("empty", List [])])
