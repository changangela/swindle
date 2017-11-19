module Error where
import Syntax
import Control.Monad.Except
import Text.ParserCombinators.Parsec.Error

data SchemeError 
  = NumArgs Integer [SchemeVal]
  | TypeMismatch String SchemeVal
  | Parser ParseError
  | BadSpecialForm String SchemeVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

instance Show SchemeError where
  show (NumArgs expected found) = "expected " ++ show expected ++ " args, found values " ++ unwordsList found
  show (TypeMismatch expected found) = "invalid type: expected " ++ expected ++ ", found " ++ show found
  show (Parser parseErr) = "parse error at " ++ show parseErr
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction message func) = message ++ ": " ++ show func
  show (UnboundVar message varName) = message ++ ": " ++ varName
  show (Default message) = message

-- instance Error SchemeError where
--   noMsg = Default "an error has occured"
--   strMsg = Default

type ThrowsError = Either SchemeError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
