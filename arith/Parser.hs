module Parser (
  parseLn
) where

import Syntax

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Expr as Expr
import qualified Text.Parsec.Token as Token

import Data.Functor.Identity

langDef :: Token.LanguageDef ()
langDef = Token.LanguageDef
  { Token.commentStart    = "{-"
  , Token.commentEnd      = "-}"
  , Token.commentLine     = "--"
  , Token.nestedComments  = True
  , Token.identStart      = letter
  , Token.identLetter     = alphaNum <|> oneOf "_'"
  , Token.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Token.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Token.reservedNames   = []
  , Token.reservedOpNames = []
  , Token.caseSensitive   = True
  }

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Token.parens lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

semiSep :: Parser a -> Parser [a]
semiSep = Token.semiSep lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

prefixOp :: String -> (a -> a) -> Expr.Operator String () Identity a
prefixOp s f = Expr.Prefix (reservedOp s >> return f)


-- Prefix operators
table :: Expr.OperatorTable String () Identity Expression
table = [
    [
      prefixOp "succ" Succ
    , prefixOp "pred" Pred
    , prefixOp "iszero" IsZero
    ]
  ]


-- if/then/else
ifthen :: Parser Expression
ifthen = do
  reserved "if"
  cond <- expr
  reservedOp "then"
  tr <- expr
  reserved "else"
  fl <- expr
  return (If cond tr fl)

-- Constants
true, false, zero :: Parser Expression
true  = reserved "true"  >> return Tr
false = reserved "false" >> return Fl
zero  = reservedOp "0"   >> return Zero

expr :: Parser Expression
expr = Expr.buildExpressionParser table factor

factor :: Parser Expression
factor =
      true
  <|> false
  <|> zero
  <|> ifthen
  <|> parens expr

contents :: Parser a -> Parser a
contents p = do
  Token.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expression]
toplevel = semiSep expr

parseLn :: String -> Either ParseError Expression
parseLn line = parse (contents expr) "stdin" line
