module Eval where
import Syntax
import Error
import Control.Monad.Except

eval :: RacketVal -> ThrowsError RacketVal
eval val@(Atom "empty") = return $ List []
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "if", cond, thens, elses]) =
  do
    result <- eval cond
    case result of
      Bool False -> eval elses
      otherwise -> eval thens
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval val@(Cond question answer) = return val
eval badForm = throwError $ BadSpecialForm "unrecognized special form" badForm

apply :: String -> [RacketVal] -> ThrowsError RacketVal
apply func args = maybe (throwError $ NotFunction "unrecognized primitive function args" func)
                    ($ args)
                    (lookup func primitives)

primitives :: [(String, [RacketVal] -> ThrowsError RacketVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("and", boolBoolBinop (&&)),
              ("or", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("list", list),
              ("eqv?", eqv),
              ("equal?", equal),
              ("eq?", eq),
              ("cond", cond),
              ("first", car),
              ("rest", cdr)]

cond :: [RacketVal] -> ThrowsError RacketVal
cond [] = throwError $ Default "cond: all question results were false"
cond ((Cond question answer):rest) = do
  let result = eval question
  case result of
    Left err -> throwError err
    Right (Bool True) -> (eval answer)
    Right (Bool False) -> (cond rest)
cond (badArg:rest) = throwError $ TypeMismatch "cond" badArg



eqv :: [RacketVal] -> ThrowsError RacketVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList


equal :: [RacketVal] -> ThrowsError RacketVal
equal [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
equal [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
equal [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
equal [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
-- eqv [(DottedList xfirst xrest), (DottedList yfirst yrest)] = eqv [List $ xfirst ++ [xlast], List $ yfirst ++ [ylast]]
equal [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && 
                                          (all eqvPair $ zip arg1 arg2)
                                            where eqvPair (x1, x2) = case eqv [x1, x2] of
                                                                      Left err -> False
                                                                      Right (Bool val) -> val
equal [_, _] = return $ Bool False
equal badArgList = throwError $ NumArgs 2 badArgList

eq :: [RacketVal] -> ThrowsError RacketVal
eq [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eq [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eq [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eq [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eq [_, _] = return $ Bool False
eq badArgList = throwError $ NumArgs 2 badArgList


list :: [RacketVal] -> ThrowsError RacketVal
list args = return $ List args

car :: [RacketVal] -> ThrowsError RacketVal
car [List (x : y)] = return x
-- car [DottedList (x : y) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [RacketVal] -> ThrowsError RacketVal
cdr [List (x : y)] = return $ List y
-- cdr [DottedList [_] y] = return y
-- cdr [DottedList (_ : x) y] = return DottedList x y
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [RacketVal] -> ThrowsError RacketVal
cons [x, List []] = return $ List [x]
cons [x, List y] = return $ List $ x : y
-- cons [x, DottedList y z] = return $ DottedList (x : y) z 
-- cons [x, y] = return $ DottedList [x] y
cons badArgList = throwError $ NumArgs 2 badArgList
  

numericBinop :: (Integer -> Integer -> Integer) -> [RacketVal] -> ThrowsError RacketVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: RacketVal -> ThrowsError Integer
unpackNum (Number n) = return n
-- unpackNum (String n) = let parsed = reads n::[(Integer, String)] in
--                           if null parsed
--                             then 0
--                             else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

boolBinop :: (RacketVal -> ThrowsError a) -> (a -> a -> Bool) -> [RacketVal] -> ThrowsError RacketVal
boolBinop unpacker op args =  if length args /= 2 then throwError $ NumArgs 2 args
                              else do
                                left <- unpacker $ args !! 0
                                right <- unpacker $ args !! 1
                                return $ Bool $ left `op` right

-- numBoolBinop :: (Integer -> Integer -> Bool) -> [RacketVal] -> ThrowsError RacketVal
-- numBoolBinop op params =  if length params < 2 then throwError $ NumArgs 2 params
--                           else mapM unpackNum params >>= return . Bool . (\adjPair -> (all adjPair) where adjPair (x1, x2) = x1 `op` x2) . (\args -> zip args (tail args))

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
-- boolBoolBinop = boolBinop unpackBool

boolBoolBinop :: (Bool -> Bool -> Bool) -> [RacketVal] -> ThrowsError RacketVal
boolBoolBinop op params = if length params < 2 then throwError $ NumArgs 2 params
                          else mapM unpackBool params >>= return . Bool . foldl1 op


unpackStr :: RacketVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: RacketVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool
