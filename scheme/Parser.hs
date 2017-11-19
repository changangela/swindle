module Parser where
import Syntax
import Error
import System.Environment
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> ThrowsError SchemeVal
readExpr input = case parse parseExpr "scheme" input of
  Left err -> throwError $ Parser err
  Right val -> return val

spaces :: Parser ()
spaces = skipMany1 space

parseAtom :: Parser SchemeVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "true" -> Bool True
    "false" -> Bool False
    _    -> Atom atom


parseNumber :: Parser SchemeVal
parseNumber = liftM (Number . read) (many1 digit)

parseString :: Parser SchemeVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return (String x) -- $ String x

parseExpr :: Parser SchemeVal
parseExpr
  = parseAtom
  <|> parseNumber
  <|> parseString
  <|> parseQuoted
  <|> do
    char '('
    x <- try parseList <|> parseDottedList
    -- x <- parseList
    char ')'
    return x

parseList :: Parser SchemeVal
parseList = do liftM List $ sepBy parseExpr spaces

-- syntactic sugar with ',' character: (a (dotted . list) test)
parseDottedList :: Parser SchemeVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

-- syntactic sugar with '\'': '(1 2 3 4)
parseQuoted :: Parser SchemeVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]