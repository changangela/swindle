module Parser where
import Syntax
import Error
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> ThrowsError RacketVal
readExpr input = case parse parseExpr "racket" input of
  Left err -> throwError $ Parser err
  Right val -> return val

spaces :: Parser ()
spaces = skipMany1 space

parseAtom :: Parser RacketVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "true" -> Bool True
    "false" -> Bool False
    _    -> Atom atom


parseNumber :: Parser RacketVal
parseNumber = liftM (Number . read) (many1 digit)

parseString :: Parser RacketVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return (String x) -- $ String x

parseExpr :: Parser RacketVal
parseExpr
  = parseAtom
  <|> parseNumber
  <|> parseString
  <|> parseQuoted
  <|> do
    char '('
    -- x <- try parseList <|> parseDottedList
    x <- parseList
    char ')'
    return x
  <|> do
    char '['
    x <- parseCond
    char ']'
    return x

parseList :: Parser RacketVal
parseList = do liftM List $ sepBy parseExpr spaces

-- syntactic sugar with ',' character: (a (dotted . list) test)
-- parseDottedList :: Parser RacketVal
-- parseDottedList = do
--   head <- endBy parseExpr spaces
--   tail <- char '.' >> spaces >> parseExpr
--   return $ DottedList head tail

-- syntactic sugar with '\'': '(1 2 3 4)
parseQuoted :: Parser RacketVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseCond :: Parser RacketVal
parseCond = do 
  x <- parseExpr
  spaces
  y <- parseExpr
  return $ Cond x y
