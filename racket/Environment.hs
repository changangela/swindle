module Environment where
import Data.IORef
import Syntax

type Env = IORef [(String, IORef RacketVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ErrorT RacketError IO
