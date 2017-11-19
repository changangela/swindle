module Error (module Error, module Control.Monad.Except) where
import Syntax
import Control.Monad.Except
import Text.ParserCombinators.Parsec.Error

data RacketError 
  = NumArgs Integer [RacketVal]
  | TypeMismatch String RacketVal
  | Parser ParseError
  | BadSpecialForm String RacketVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

instance Show RacketError where
  show (NumArgs expected found) = "expected " ++ show expected ++ " args, found values " ++ unwordsList found
  show (TypeMismatch expected found) = "invalid type: expected " ++ expected ++ ", found " ++ show found
  show (Parser parseErr) = "parse error at " ++ show parseErr
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction message func) = message ++ ": " ++ show func
  show (UnboundVar message varName) = message ++ ": " ++ varName
  show (Default message) = message

-- instance Error RacketError where
--   noMsg = Default "an error has occured"
--   strMsg = Default

type ThrowsError = Either RacketError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

