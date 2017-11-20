module Eval where
import Syntax
-- import Debug.Trace

eval :: Env -> RacketVal -> IOThrowsError RacketVal
eval env val@(Atom id) = getVar env id
eval env val@(List ((Atom "cond"):conds)) = cond env conds
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (List [Atom "if", cond, thens, elses]) =
  do
    result <- eval env cond
    case result of
      Bool False -> eval env elses
      otherwise -> eval env thens
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List [Atom "quote", val]) = return val
eval env (List (Atom "define" : List(Atom var : params) : body)) = makeNormalFunc env params body >>= defineVar env var
-- eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
--   makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) = makeNormalFunc env params body
-- eval env (List (Atom "lambda" : DottedList params varargs : body)) =
--   makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) = makeVarargs varargs env [] body
eval env (List (function : args)) = do
     func <- eval env function
     argVals <- mapM (eval env) args
     apply func argVals
eval env badForm = throwError $ BadSpecialForm "unrecognized special form" badForm

apply :: RacketVal -> [RacketVal] -> IOThrowsError RacketVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args = 
  if num params /= num args && varargs == Nothing
     then throwError $ NumArgs (num params) args
     else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
  where remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) body 
        bindVarArgs arg env = case arg of
            Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
            Nothing -> return env 

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . showVal

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives)
                    where makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)
-- apply :: String -> [RacketVal] -> ThrowsError RacketVal
-- apply func args = maybe (throwError $ NotFunction "unrecognized primitive function args" func)
--                     ($ args)
--                     (lookup func primitives)

-- initialize some keywords
racketEnv :: IO Env
racketEnv = primitiveBindings >>= (flip bindVars [("empty", List [])])


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
              ("first", car),
              ("rest", cdr)]

cond :: Env -> [RacketVal] -> IOThrowsError RacketVal
cond env [] = throwError $ Default "cond: all question results were false"
cond env ((Cond question answer):rest) = do
  result <- eval env question
  case result of
    (Bool False) -> (cond env rest)
    (Bool True) -> eval env answer
    _ -> throwError $ TypeMismatch "bool" result
cond env ((List [question, answer]):rest) = cond env ((Cond question answer):rest)
cond env badArgList = throwError $ TypeMismatch "cond" (head badArgList)


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
-- numericBinop op [] = throwError $ NumArgs 2 []
-- numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
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
