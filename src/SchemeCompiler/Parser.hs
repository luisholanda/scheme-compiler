module SchemeCompiler.Parser where

import           Control.Monad
import           Numeric
import           Text.ParserCombinators.Parsec hiding (spaces)

import           SchemeCompiler.Lisp
import           SchemeCompiler.Utils
import           SchemeCompiler.Shower

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow  parser input =
  case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseNumber
        <|> try parseBool
        <|> try parseChar
        <|> try parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ many1 (noneOf "\\\"\n\r\t") <|> escapeChars
  char '"'
  return . String $ concat x

escapeChars :: Parser String
escapeChars = do
  char '\\'
  x <- oneOf "\\\"nrt"
  case x of
    '\\' -> return [x]
    '"'  -> return [x]
    'n'  -> return "\n"
    'r'  -> return "\r"
    't'  -> return "\t"

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many $ letter <|> digit <|> symbol
  let atom = first : rest
  return . Atom $ atom

parseBool :: Parser LispVal
parseBool = do
  string "#"
  x <- oneOf "tf"
  return $
    case x of
      't' -> Bool True
      'f' -> Bool False

parseChar :: Parser LispVal
parseChar = do
  string "#\\"
  x <- parseCharName <|> anyChar
  return ( Character x )

parseCharName :: Parser Char
parseCharName = do
  x <- try (string "newline" <|> string "space")
  return $ case x of
    "space"   -> ' '
    "newline" -> '\n'

parseNumber :: Parser LispVal
parseNumber = try parseDigital1
          <|> try parseDigital2
--          <|> try parseFloat
          <|> try parseHex
          <|> try parseOct
          <|> parseBin
  where
    parseDigital1 = do
      x <- many1 digit
      (return . Number . read) x

    parseDigital2 = string "#d" >> parseDigital1

--    parseFloat = return

    parseHex = do
      string "#x"
      x <- many hexDigit
      return . Number . hex2dig $ x

    parseOct = do
      string "#o"
      x <- many octDigit
      return . Number . oct2dig $ x

    parseBin = do
      string "#b"
      x <- many1 (oneOf "10")
      return . Number . bin2dig $ x

parseList :: Parser LispVal
parseList = List `fmap` sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  h <- endBy parseExpr spaces
  t <- char '.' >> spaces >> parseExpr
  return $ DottedList h t

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]
