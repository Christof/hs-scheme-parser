import           Control.Monad
import           System.Environment
import           Text.ParserCombinators.Parsec hiding (spaces)

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal]
               LispVal
  | Number Integer
  | String String
  | Bool Bool

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $
    case atom of
      "#t" -> Bool True
      "#f" -> Bool False
      _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many (noneOf "\"")
  _ <- char '"'
  return $ String x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseExpr :: Parser LispVal
parseExpr =
  parseAtom <|> parseString <|> parseNumber <|> do
    _ <- char '('
    x <- try parseList <|> parseDottedList
    _ <- char ')'
    return x

readExpr :: String -> String
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _  -> "Found value!"

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
