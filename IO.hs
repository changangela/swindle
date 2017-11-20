module IO where
import Syntax

ioPrimitives :: [(String, [RacketVal] -> IOThrowsError RacketVal)]

ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

applyProc :: [RacketVal] -> IOThrowsError RacketVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makePort :: IOMode -> [RacketVal] -> IOThrowsError RacketVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [RacketVal] -> IOThrowsError RacketVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

readProc :: [RacketVal] -> IOThrowsError RacketVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [RacketVal] -> IOThrowsError RacketVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [RacketVal] -> IOThrowsError RacketVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [RacketVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [RacketVal] -> IOThrowsError RacketVal
readAll [String filename] = liftM List $ load filename