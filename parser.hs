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

parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr =
  parseAtom <|> parseString <|> parseNumber <|> parseQuoted <|> do
    _ <- char '('
    x <- try parseList <|> parseDottedList
    _ <- char ')'
    return x

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) =
  "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

instance Show LispVal where
  show = showVal

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _          = 0

isString :: [LispVal] -> LispVal
isString [String _] = Bool True
isString _          = Bool False

isNumber :: [LispVal] -> LispVal
isNumber [Number _] = Bool True
isNumber _          = Bool False

isSymbol :: [LispVal] -> LispVal
isSymbol [Atom _] = Bool True
isSymbol _        = Bool False

isBoolean :: [LispVal] -> LispVal
isBoolean [Bool _] = Bool True
isBoolean _        = Bool False

isList :: [LispVal] -> LispVal
isList [List _] = Bool True
isList _        = Bool False

primitives :: [(String, [LispVal] -> LispVal)]
primitives =
  [ ("+", numericBinop (+))
  , ("-", numericBinop (-))
  , ("*", numericBinop (*))
  , ("/", numericBinop div)
  , ("mod", numericBinop mod)
  , ("quotient", numericBinop quot)
  , ("remainder", numericBinop rem)
  , ("string?", isString)
  , ("number?", isNumber)
  , ("symbol?", isSymbol)
  , ("boolean?", isBoolean)
  , ("list?", isList)
  ]

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

eval :: LispVal -> LispVal
eval val@(String _)             = val
eval val@(Number _)             = val
eval val@(Bool _)               = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func:args))    = apply func $ map eval args

readExpr :: String -> LispVal
readExpr input =
  case parse parseExpr "lisp" input of
    Left err  -> String $ "No match: " ++ show err
    Right val -> val

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
