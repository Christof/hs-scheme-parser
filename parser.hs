import           Control.Monad
import           Control.Monad.Except
import           Data.Array
import           Numeric
import           System.Environment
import           Text.ParserCombinators.Parsec hiding (spaces)

data LispVal
  = Atom String
  | List [LispVal]
  | Vector (Array Int LispVal)
  | DottedList [LispVal]
               LispVal
  | Number Integer
  | Float Double
  | Character Char
  | String String
  | Bool Bool

data LispError
  = NumArgs Integer
            [LispVal]
  | TypeMismatch String
                 LispVal
  | Parser ParseError
  | BadSpecialForm String
                   LispVal
  | NotFunction String
                String
  | UnboundVar String
               String
  | Default String

showError :: LispError -> String
showError (NumArgs expected found) =
  "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) =
  "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (Default message) = "Error: " ++ message

instance Show LispError where
  show = showError

type ThrowsError = Either LispError

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ Atom atom

parseDecimal :: Parser LispVal
parseDecimal = many1 digit >>= (return . Number . read)

parseExplicit :: String -> Parser Char -> (String -> Integer) -> Parser LispVal
parseExplicit prefix digitParser conversion = do
  _ <- try $ string prefix
  x <- many1 digitParser
  (return . Number . conversion) x

parseExplicitDecimal :: Parser LispVal
parseExplicitDecimal = parseExplicit "#d" digit read

hexToDigit :: (Eq a, Num a) => String -> a
hexToDigit x = fst $ readHex x !! 0

parseHex :: Parser LispVal
parseHex = parseExplicit "#h" hexDigit hexToDigit

octToDigit :: (Eq a, Num a) => String -> a
octToDigit x = fst $ readOct x !! 0

parseOct :: Parser LispVal
parseOct = parseExplicit "#o" octDigit octToDigit

binToDigit :: [Char] -> Integer
binToDigit = binToDigit' 0

binToDigit' :: Integer -> [Char] -> Integer
binToDigit' digint "" = digint
binToDigit' digint (x:xs) =
  let old =
        2 * digint +
        (if x == '0'
           then 0
           else 1)
   in binToDigit' old xs

binDigit = oneOf "10"

parseBin :: Parser LispVal
parseBin = parseExplicit "#b" binDigit binToDigit

parseNumber :: Parser LispVal
parseNumber =
  parseDecimal <|> parseExplicitDecimal <|> parseHex <|> parseOct <|> parseBin

parseFloat :: Parser LispVal
parseFloat = do
  x <- many1 digit
  _ <- char '.'
  y <- many1 digit
  (return . Float . fst . head . readFloat) (x ++ "." ++ y)

escapedChars :: Parser Char
escapedChars = do
  _ <- char '\\'
  x <- oneOf "\\\"nrt"
  return $
    case x of
      'n' -> '\n'
      'r' -> '\r'
      't' -> '\t'
      _   -> x -- for \\ and "

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many $ escapedChars <|> noneOf "\\\""
  _ <- char '"'
  return $ String x

parseBool :: Parser LispVal
parseBool = do
  _ <- char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseChar :: Parser LispVal
parseChar = do
  _ <- try $ string "#\\"
  x <-
    try (string "newline" <|> string "space") <|> do
      x <- anyChar
      notFollowedBy alphaNum
      return [x]
  return $
    Character $
    case x of
      "space"   -> ' '
      "newline" -> '\n'
      otherwise -> (x !! 0)

parseListElements :: Parser LispVal
parseListElements = liftM List $ sepBy parseExpr spaces

parseDottedListElements :: Parser LispVal
parseDottedListElements = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseListOrDottedList :: Parser LispVal
parseListOrDottedList = do
  _ <- string "("
  x <- try parseListElements <|> parseDottedListElements
  _ <- char ')'
  return x

parseVectorElements :: Parser LispVal
parseVectorElements = do
  arrayValues <- sepBy parseExpr spaces
  return $ Vector (listArray (0, (length arrayValues - 1)) arrayValues)

parseVector :: Parser LispVal
parseVector = do
  _ <- string "#("
  x <- parseVectorElements
  _ <- char ')'
  return x

parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
  _ <- char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
  _ <- char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

parseExpr :: Parser LispVal
parseExpr =
  parseAtom <|> try parseFloat <|> try parseNumber <|> try parseBool <|>
  try parseChar <|>
  parseString <|>
  parseQuoted <|>
  parseQuasiQuoted <|>
  parseUnQuote <|>
  parseVector <|>
  parseListOrDottedList

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Character contents) = "#\\" ++ [contents]
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) =
  "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Vector array) = "(" ++ unwordsList (elems array) ++ ")"

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

isCharacter :: [LispVal] -> LispVal
isCharacter [Character _] = Bool True
isCharacter _             = Bool False

isNumber :: [LispVal] -> LispVal
isNumber [Number _] = Bool True
isNumber _          = Bool False

isFloat :: [LispVal] -> LispVal
isFloat [Float _] = Bool True
isFloat _         = Bool False

isSymbol :: [LispVal] -> LispVal
isSymbol [Atom _] = Bool True
isSymbol _        = Bool False

isBoolean :: [LispVal] -> LispVal
isBoolean [Bool _] = Bool True
isBoolean _        = Bool False

isList :: [LispVal] -> LispVal
isList [List _] = Bool True
isList _        = Bool False

symbolToString :: [LispVal] -> LispVal
symbolToString [Atom s] = String s
symbolToString _        = String ""

stringToSymbol :: [LispVal] -> LispVal
stringToSymbol [String s] = Atom s
stringToSymbol _          = Atom ""

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
  , ("char?", isCharacter)
  , ("number?", isNumber)
  , ("real?", isFloat)
  , ("symbol?", isSymbol)
  , ("boolean?", isBoolean)
  , ("list?", isList)
  , ("symbol->string", symbolToString)
  , ("string->symbol", stringToSymbol)
  ]

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

eval :: LispVal -> LispVal
eval val@(String _)             = val
eval val@(Character _)          = val
eval val@(Number _)             = val
eval val@(Float _)              = val
eval val@(Bool _)               = val
eval val@(Vector _)             = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func:args))    = apply func $ map eval args
eval val@(List _)               = val

readExpr :: String -> LispVal
readExpr input =
  case parse parseExpr "lisp" input of
    Left err  -> String $ "No match: " ++ show err
    Right val -> val

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
